namespace Mork.Mapping 

module internal MappingOntology =

    open System
    open System.Collections.Generic
    open VDS.RDF
    open VDS.RDF.Ontology
    open VDS.RDF.Skos
    open Mork.Vocab

    type MorkProp =
        | ObjectProp of string * Lazy<MorkNode list>
        | DataProp of string * string

    and MorkNode =
        { Uri      : string
          Types    : MorkConcept list
          Annotations : MorkAnnotation list
          Props    : MorkProp list }
        override this.ToString() = this.Uri

    let private annotationOfString (propUri:string) (value:string) =
        MorkAnnotations.TryGetValue(propUri) |> function
            | true, ctor -> ctor value
            | false, _ -> CustomAnnotation(propUri, value)

    let nodeUriString (n:INode) =
        match n with
        | :? IUriNode as uri -> uri.Uri.AbsoluteUri
        | :? IBlankNode as b -> "_:" + b.InternalID
        | _ -> n.ToString()

    let literalString (n:INode) =
        match n with
        | :? ILiteralNode as lit -> lit.Value
        | _ -> n.ToString()

    type MorkGraph(mork:OntologyGraph, skos:SkosGraph) =
        let nodeCache = Dictionary<string, Lazy<MorkNode>>()

        // Materialize a node and its properties and annotations
        member this.MaterialiseNode (indiv:OntologyResource) : MorkNode =
            let uri = nodeUriString indiv.Resource

            // Types (rdf:type)
            let types =
                indiv.Types
                |> Seq.choose (function
                    | :? IUriNode as uriNode -> Some(classifyType uriNode.Uri.AbsoluteUri)
                    | _ -> None)
                |> Seq.distinct
                |> Seq.toList

            let annotationTriples =
                mork.Triples.WithSubject(indiv.Resource)
                |> Seq.fold (fun acc t ->
                    match t.Predicate with
                    | :? IUriNode as p ->
                        let predUri = p.Uri.AbsoluteUri
                        if predUri.StartsWith Prefixes.Dcterms ||
                           MorkAnnotations.ContainsKey predUri
                        then
                            (predUri, literalString t.Object) :: acc
                        else acc
                    | _ -> acc
                ) []
                |> Seq.ofList
                |> Seq.distinct

            let annotations =
                Seq.map (fun (prop, value) -> annotationOfString prop value) annotationTriples

            let objProps =
                seq {
                    for t in mork.Triples.WithSubject(indiv.Resource) do
                        match t.Predicate, t.Object with
                        | (:? IUriNode as p), (:? IUriNode as o) ->
                            let propName = p.Uri.AbsoluteUri
                            if not (
                                propName.StartsWith Prefixes.Dcterms ||
                                propName.StartsWith("http://www.w3.org/2000/01/rdf-schema#")
                            )
                            then
                                yield propName, mork.CreateOntologyResource(t.Object)
                        | _ -> ()
                }
                |> Seq.groupBy fst
                |> Seq.map (fun (prop, targets) ->
                    let values =
                        targets
                        |> Seq.map (snd >> (fun r -> this.GetOrMaterialiseNode r))
                        |> Seq.toList
                    ObjectProp(prop, lazy values)
                )

           
            // TODO: we need to rethink how to handle properties 
            // One option is to use the known predicates to map them properly
            // Another is to use the ontology API to determine this, however we won't know what they DO in that case
            // So I'm leaning towards coupling this code with the ontology, since any changes would be breaking anyway
            
            //for t in mork.OwlObjectProperties do
                //t.Resource
                
        
            // Data Properties
            let dataProps =
                seq {
                    for t in mork.Triples.WithSubject(indiv.Resource) do
                        match t.Predicate, t.Object with
                        | (:? IUriNode as p), (:? ILiteralNode as lit) ->
                            let propName = p.Uri.AbsoluteUri
                            if not (propName.StartsWith("http://www.w3.org/2004/02/skos/core#") ||
                                    propName.StartsWith("http://www.w3.org/2000/01/rdf-schema#") ||
                                    propName.StartsWith("http://purl.org/dc/terms/"))
                            then
                                yield DataProp(propName, lit.Value)
                        | _ -> ()
                }

            let props = Seq.append objProps dataProps |> Seq.toList

            { Uri = uri; Types = types; Annotations = Seq.toList annotations; Props = props }

        // Memoizing node creation for interlinking
        member this.GetOrMaterialiseNode (indiv:OntologyResource) : MorkNode =
            let uri = nodeUriString indiv.Resource
            match nodeCache.TryGetValue(uri) with
            | true, lazyNode -> lazyNode.Value
            | false, _ ->
                let lazyNew = lazy (this.MaterialiseNode indiv)
                nodeCache.Add(uri, lazyNew)
                lazyNew.Value

        // Get all top-level representations (via skos:topConceptOf, using SKOS API)
    (*     member this.GetTopLevelRepresentations() =
            skos.Concepts
            |> Seq.filter (fun c -> c.IsTopConcept)
            |> Seq.map (fun c -> OntologyResource(ont, c.Node))
            |> Seq.map this.GetOrMaterialiseNode
            |> Seq.distinctBy (fun n -> n.Uri)
            |> Seq.toList
    *)
        /// Recursively collect all related mappings for a node
        member this.RecursivelyCollectMappings(start:MorkNode) : MorkNode list =
            let rec loop (visited: HashSet<string>) (acc: MorkNode list) (current: MorkNode) =
                if visited.Contains(current.Uri) then acc
                else
                    visited.Add(current.Uri) |> ignore
                    let direct =
                        current.Props
                        |> List.choose (function | ObjectProp(_, lz) -> Some lz.Value | _ -> None)
                        |> List.collect id
                        |> List.filter (fun n -> n.Types |> List.exists (function DataMapping -> true | _ -> false))
                    List.fold (loop visited) (current :: acc) direct
            loop (HashSet()) [] start |> List.rev

        member this.GetIndividualsOfType<'T when 'T :> NamedConcept> (t:'T) =
            let typeUri = FQName t
            let typeNode = mork.CreateUriNode(Uri typeUri)
            mork.Triples.WithPredicateObject(mork.CreateUriNode(Uri RdfType), typeNode)
            |> Seq.map (fun t -> mork.CreateOntologyResource t.Subject)
            |> Seq.map this.GetOrMaterialiseNode
            |> Seq.toList

    let loadMorkOntology (filename:string) =
        let g = new OntologyGraph()
        g.LoadFromFile(filename)
        let baseUri = g.BaseUri
        g.UriFactory.InternUris <- true
        g.NamespaceMap.AddNamespace("mork", Uri Prefixes.Mork)
        let skos = new SkosGraph(g)
        MorkGraph(g, skos)


    // -------------------
    // Example Usage (commented)
    // -------------------

    (*
    open MorkOntologyDotNetRdf

    let ctx = loadMorkOntology "your-mork-ontology.ttl"

    printfn "Top Representations:"
    let topReps = ctx.GetTopLevelRepresentations()
    for rep in topReps do
        printfn "  %s" rep.Uri
        printfn "    Types: %A" rep.Types
        printfn "    Annotations:"
        for ann in rep.Annotations do printfn "      %A" ann

        let allMappings = ctx.RecursivelyCollectMappings rep
        printfn "    All related mappings:"
        for mapping in allMappings do
            printfn "      - %s (%A)" mapping.Uri mapping.Types
            if mapping.Annotations.Length > 0 then
                printfn "        Annotations:"
                for ann in mapping.Annotations do printfn "          %A" ann
    *)