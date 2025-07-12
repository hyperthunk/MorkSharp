// This script parses the Mork.ttl ontology and generates F# let bindings for all Mork object properties
// as well as a MorkProperties map suitable for inclusion in MorkVocab.fs.
// Usage: dotnet fsi generateMorkProperties.fsx > Properties.fs
// Requires: System.Text.RegularExpressions

open System
open System.IO
open System.Text.RegularExpressions

// Cross platform path
let morkTtlPath =
    let rel = [| "paket-files"; "hyperthunk"; "mork"; "spec"; "Mork.ttl" |]
    let baseDir = __SOURCE_DIRECTORY__
    let path = Path.Combine(Array.append [|baseDir|] rel)
    if File.Exists path then path
    else
        // Try with unix style slashes if invoked from *nix shell
        let unixPath = String.Join("/", rel)
        let alt = Path.Combine(baseDir, unixPath)
        if File.Exists alt then alt
        else failwithf "Could not find Mork.ttl at %s" path

let morkUri = "http://www.nebularis.org/ontologies/Mork#"

let input = File.ReadAllText morkTtlPath

// Regex to match object properties of the form ":propertyName rdf:type owl:ObjectProperty ;"
// and their subPropertyOf (for hierarchy), and label.
let reObjProp = new Regex(@"^:(\w+)\s+rdf:type\s+owl:ObjectProperty\s*;(.+?)(?:\.\s*|$)", RegexOptions.Multiline ||| RegexOptions.Singleline)

let reSubProp = new Regex(@"rdfs:subPropertyOf\s+(:\w+|<[^>]+>)\s*;", RegexOptions.Compiled)
let reLabel = new Regex(@"rdfs:label\s+""([^""]+)""\s*;", RegexOptions.Compiled)

let reDeprecated = new Regex(@"owl:deprecated\s+""[^""]+""", RegexOptions.Compiled)

let reInverseOf = new Regex(@"owl:inverseOf\s+(:\w+|<[^>]+>)\s*;", RegexOptions.Compiled)

let parseObjProps (ttl:string) =
    [
        for m in reObjProp.Matches(ttl) do
            let name = m.Groups.[1].Value
            let body = m.Groups.[2].Value

            let subPropOf =
                match reSubProp.Match(body) with
                | s when s.Success ->
                    let v = s.Groups.[1].Value
                    if v.StartsWith(":") then Some(v.TrimStart(':'))
                    else None
                | _ -> None

            let label =
                match reLabel.Match(body) with
                | l when l.Success -> Some(l.Groups.[1].Value)
                | _ -> None

            let inverseOf =
                match reInverseOf.Match(body) with
                | i when i.Success -> Some(i.Groups.[1].Value.TrimStart(':'))
                | _ -> None

            let deprecated = reDeprecated.IsMatch(body)
            yield name, subPropOf, label, inverseOf, deprecated
    ]

let allProps = parseObjProps input

let skosObjectProperties = Set.ofList [
    "skos:related";
    "skos:broader";
    "skos:narrower";
    "skos:exactMatch";
    "skos:closeMatch";
    "skos:relatedMatch";
    "skos:inScheme";
    "skos:member";
    "skos:hasTopConcept";
    "skos:topConceptOf";
    "related";
    "broader";
    "narrower";
    "exactMatch";
    "closeMatch";
    "relatedMatch";
    "inScheme";
    "member";
    "hasTopConcept";
    "topConceptOf"
]

// Choose  properties to ignore for let bindings, but keep in map as CustomProperty
let morkOnly (name:string) = not(
    name.StartsWith("rdf") || name.StartsWith("owl") || name.StartsWith("rdfs")
)   

let linking = Set.ofList [
    "format";
    "mappingFor";
    "hasConcept";
    "hasMapping";
    "indicativeMapping";
    "memberOf";
    "representationOf";
    "representedAs";
    "relatedProperty";
    "siblingMapping";
    "mappingScheme";
    "ontologicalScheme";
    "representationScheme";
    "conceptScheme";
]

let close = Set.ofList [
    "lexicalMatch";
    "semanticMatch";
    "structuralMatch";
    "partialMatch";
    "possibleMatch";
]

let narrow = Set.ofList [
    "elementType";
    "memberProperty";
]

let semanticDistanceFromName (name:string) =
    match name.ToLowerInvariant() with
        | n when n.Contains "broad" -> "Broad"
        | n when n.Contains "exact" || n.Contains "inverserbox" -> "Exact"
        | n when n.Contains "narrow" || narrow.Contains name -> "Narrow"
        | n when n.Contains "close" || close.Contains name -> "Close"
        | _ -> "NA" // Default

let predicateSemanticsFromName (name:string) =
    if name.Contains("Associative", StringComparison.OrdinalIgnoreCase) then "Associative"
    elif name.Contains("Applicative", StringComparison.OrdinalIgnoreCase) then "UpdateContext"
    elif name.Contains("Composite", StringComparison.OrdinalIgnoreCase) then "Compositional"
    elif name.Contains("Composition", StringComparison.OrdinalIgnoreCase) then "Compositional"
    elif name.Contains("CategoryMatch", StringComparison.OrdinalIgnoreCase) then "Categorical"
    elif name.Contains("Identity", StringComparison.OrdinalIgnoreCase) then "Identity"
    elif name.Contains("Indicative", StringComparison.OrdinalIgnoreCase) ||
         name.Contains("InverseRBoxMatch", StringComparison.OrdinalIgnoreCase) ||
         name.Contains("PathMatch", StringComparison.OrdinalIgnoreCase) ||
         name.Contains("ReferenceDataPath", StringComparison.OrdinalIgnoreCase) ||
         close.Contains(name) then "Indicative"
    elif name.Contains("Exact", StringComparison.OrdinalIgnoreCase) &&
         name.Contains("Match", StringComparison.OrdinalIgnoreCase) then "Indicative"
    elif name.Contains("Hypothesis", StringComparison.OrdinalIgnoreCase) ||
         name.Contains("Hypotheses", StringComparison.OrdinalIgnoreCase) ||
         name.Contains("Dependent", StringComparison.OrdinalIgnoreCase) ||
         name.Contains("Deferred", StringComparison.OrdinalIgnoreCase) then "Dependent"
    elif name.Contains("Template", StringComparison.OrdinalIgnoreCase) ||
         name.Contains("Mapping", StringComparison.OrdinalIgnoreCase) ||
         name.Contains("Format", StringComparison.OrdinalIgnoreCase) then "Configuration"
    else "NonApplicable" // Default

let nodeSemanticsFromName (name:string) =
    if name.Contains("Related", StringComparison.OrdinalIgnoreCase) then "Related"
    elif name.Contains("Template", StringComparison.OrdinalIgnoreCase) then "Template"
    elif name.Contains("Hypothes", StringComparison.OrdinalIgnoreCase) then "Hypotheses"
    elif linking.Contains name then "UnknownOrNA"
    elif name.Contains("Mapping", StringComparison.OrdinalIgnoreCase) then "Mapping"
    else "Related"

let executionScopeFromName (name:string) =
    if name.Contains("Applicative", StringComparison.OrdinalIgnoreCase) then "Applicative"
    elif name.Contains("Deferred", StringComparison.OrdinalIgnoreCase) then "Deferred"
    elif name.Contains("Inherited", StringComparison.OrdinalIgnoreCase) then "Inherited"
    else "Local"

let camelCase (s: string) =
    if String.IsNullOrEmpty s then s
    else s.Substring(0, 1).ToUpperInvariant() + s.Substring(1)

let letBinding (name: string, subPropOf, label, inverseOf, deprecated) =
    let camelised = camelCase name
    if linking.Contains name then
        // Skip linking properties for let bindings, but keep in map
        $"    let {camelised} = LinkingProperty \"{morkUri + name}\""
    else
        // Generate let binding for Mork object property
        let d = semanticDistanceFromName name
        let p = predicateSemanticsFromName name
        let o = nodeSemanticsFromName name
        let s = executionScopeFromName name
        if name.StartsWith "skos" || skosObjectProperties.Contains name then
            $"    let {camelised} = SkosObjectProperty ({{ Distance = {d}; Predicate = {p}; Object = {o}; Scope = {s} }}, \"{name}\")"
        else
            $"    let {camelised} = ObjectProperty ({{ Distance = {d}; Predicate = {p}; Object = {o}; Scope = {s} }}, \"{name}\")"

let letBindings =
    allProps
    |> List.filter (fun (n,_,_,_,deprecated) -> morkOnly n && not deprecated)
    |> List.map letBinding

let mapEntry (name, _, _, _, deprecated) =
    let iri = morkUri + name
    if morkOnly name && not deprecated then $"        \"{iri}\", {camelCase name}"
    else $"        \"{iri}\", CustomProperty \"{iri}\""

let mapBindings =
    allProps
    |> List.map mapEntry

// Print output as a module in namespace MorkSharp, module Properties, with correct indentation
printfn "namespace MorkSharp"
printfn ""
printfn "open MorkSharp.Vocab"
printfn ""
printfn "module internal Properties ="
printfn ""
printfn "    // AUTOGENERATED - DO NOT EDIT MANUALLY"
printfn "    // Generated by generateMorkProperties.fsx"
printfn ""

for l in letBindings do printfn "%s" l

printfn ""
printfn "    let MorkProperties : Map<string, MorkProperty> ="
printfn "        ["
for m in mapBindings do printfn "%s" m
printfn "        ] |> Map.ofList"