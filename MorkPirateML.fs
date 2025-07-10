namespace MorkSharp

open System
open System.IO
open System.Text
open VDS.RDF
open VDS.RDF.Ontology
open VDS.RDF.Skos

open MorkSharp.Graph
open MorkSharp.Vocab

// You will need to add a package reference for YamlDotNet to your project for YAML serialization
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

/// In-memory representation of a YARRRML mapping
type YarrrmlPrefix = { prefix: string; ns: string }
type YarrrmlSource = string * string   // (file~syntax, selector)
type YarrrmlPredicateObject =
    | PO of string * string
    | POMapping of string * string * string   // predicate, o-mapping, mappingName
    | POMappingFn of string * string         // predicate, function
type YarrrmlSubject =
    | SValue of string
    | SMapping of string * string            // value, mappingName

type YarrrmlMapping = {
    Name: string
    Sources: YarrrmlSource list
    Subject: YarrrmlSubject
    PredicateObjects: YarrrmlPredicateObject list
}
type YarrrmlDoc = {
    Prefixes: YarrrmlPrefix list
    Mappings: YarrrmlMapping list
}

/// Utility functions for YARRRML generation
module YarrrmlUtil =
    /// Helper to create YARRRML-compliant mapping name
    let sanitizeName (iri: string) =
        // e.g., make a short name from a URI
        if String.IsNullOrWhiteSpace(iri) then "unnamed"
        else
            let safe = iri
                        .Replace("http://", "")
                        .Replace("https://", "")
                        .Replace("/", "_")
                        .Replace("#", "_")
                        .Replace(":", "_")
            if safe.Length > 64 then safe.[safe.Length-64..] else safe

    let buildPrefixList (g: OntologyGraph) =
        g.NamespaceMap.Prefixes
        |> Seq.map (fun p -> {prefix = p; ns = g.NamespaceMap.GetNamespaceUri(p).AbsoluteUri})
        |> Seq.toList

    let toSnakeCase (s: string) =
        // Replace spaces and camelCase with snake_case
        System.Text.RegularExpressions.Regex.Replace(s, "(?<=[a-z0-9])([A-Z])", "_$1").ToLower().Replace(" ", "_")

/// Core PirateML module
module PirateML =
    open YarrrmlUtil

    // Recursively walk the mapping hierarchy, producing YARRRML mappings.
    let rec generateYarrrmlMappingsRecursive
        (g: OntologyGraph)
        (mappingInd: INode)
        (visited: Set<string>) // To avoid cycles
        : YarrrmlMapping list =

        let mappingId = mappingInd.ToString()
        if visited.Contains(mappingId) then
            [] // Prevent cycles
        else
        let visited = visited.Add(mappingId)

        // Build mapping for this node
        let name = YarrrmlUtil.sanitizeName mappingId

        // Extract source, subject, etc. (stub: replace with real extraction)
        let sources = [ ("data.json~jsonpath", "$") ] // TODO: extract
        let subject = YARRRML.SValue (sprintf "%s_GenID" name) // TODO: extract
        let pos = [ YARRRML.PO("a", "contract:Contract") ] // TODO: extract

        // Find compositeNarrower child mappings
        let childMappings =
            g.GetTriplesWithSubjectPredicate(mappingInd, g.CreateUriNode(MORK_COMPOSITE_NARROWER_MAPPING))
            |> Seq.map (fun t -> t.Object)
            |> Seq.collect (fun childInd ->
                generateYarrrmlMappingsRecursive g childInd visited
            )
            |> Seq.toList

        // Build mapping for this node
        let myMapping = {
            Name = name
            Sources = sources
            Subject = subject
            PredicateObjects = pos
        }

        myMapping :: childMappings

    /// Extracts all DataMappings from a Mork ontology and generates YARRRML mappings
    let generateYarrrml (mappingGraph: MappingGraph) : YarrrmlDoc =
        let mork = mappingGraph :> obj :?> OntologyGraph
        let prefixes = buildPrefixList mork

        // Find all DataMapping individuals in the MappingScheme
        let mappings =
            mappingGraph.GetMappings()
            |> Seq.choose (fun mappingInd ->
                let ind = mappingInd.Resource :?> INode
                let indUri = ind.ToString()
                let name = sanitizeName indUri

                // Find sources (dataRef/dataInline, etc.)
                let sources =
                    // In practice, you'd look for properties like mork:dataRef, mork:dataInline
                    // For demo: just use a placeholder, but should traverse RDF for real values
                    [ ("data.json~jsonpath", "$") ] // TODO: extract actual source paths

                // Determine subject
                let subject =
                    // Try to get the IRI template or explicit value
                    // Placeholder: look for mork:conceptId or mork:identifier
                    // TODO: Traverse mappingInd for these
                    SValue (sprintf "%s_GenID12345" name)

                // Predicate-object list
                let pos =
                    // For each mapping, find PO targets (mork:exactRBoxMatch, mork:relatedMapping, etc.)
                    // Traverse owl:axioms for details in real implementation
                    [ PO ("a", "contract:Contract") ] // TODO: extract dynamically
                Some {
                    Name = name
                    Sources = sources
                    Subject = subject
                    PredicateObjects = pos
                }
            )
            |> Seq.toList

        { Prefixes = prefixes; Mappings = mappings }

    /// Serializes YarrrmlDoc to YAML
    let serializeToYaml (doc: YarrrmlDoc) : string =
        let serializer =
            SerializerBuilder()
                .WithNamingConvention(CamelCaseNamingConvention.Instance)
                .Build()
        serializer.Serialize doc

    /// Writes YarrrmlDoc to a file
    let writeToFile (filename: string) (doc: YarrrmlDoc) =
        let yaml = serializeToYaml doc
        File.WriteAllText(filename, yaml, Encoding.UTF8)

    // Optionally: pretty-print for REPL/dev
    let printDoc (doc: YarrrmlDoc) =
        printfn "%s" (serializeToYaml doc)

/// Example usage:
(*
open VDS.RDF
open VDS.RDF.Ontology

let g = new OntologyGraph()
g.LoadFromFile("Demo.ttl") // or wherever your .ttl files are

let skos = new SkosGraph()
skos.LoadFromFile("skos.ttl")

let mappingGraph = Mork.Graph.MappingGraph(g, skos)
let yarrrml = PirateML.generateYarrrml mappingGraph

PirateML.printDoc yarrrml
PirateML.writeToFile "yarrrml.yaml" yarrrml
*)
