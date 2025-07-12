namespace MorkSharp

open MorkSharp.Vocab
open FsLibLog
open FsLibLog.Types

module Graph =

    open System
    open VDS.RDF
    open VDS.RDF.Ontology
    open VDS.RDF.Parsing
    open VDS.RDF.Skos

    type NodeExprBuilder() =

        member this.Bind(m: INode, f) =
            match m with
                | :? IUriNode as uriNode -> f uriNode
                | _ -> m

        member this.Return x = x :> INode

    let withUriNodes = new NodeExprBuilder()

    let logger = LogProvider.getLoggerByType(typeof<Graph>)

    // log tweaking
    type LogScope = {
        Graph: OntologyGraph
        Skos: SkosGraph
        Context : OntologyResource
    }

    type Scalar =
        | XsdBin of byte[]
        | XsdBool of bool
        | XsdString of string
        | XsdInteger of int
        | XsdLong of int64
        | XsdFloat of float
        | XsdDecimal of decimal
        | XsdUri of Uri
        | XsdIri of Uri 
        | XsdLangLiteral of string * string // (value, language)
        | XsdDateTime of DateTime
        | Null 

    type Data = 
        | Prefix of string * string     // (prefix, namespace)
        | Namespace of string           // Just a namespace URI
        | Import of Uri                 // We might import something*
        | Vocab of Node                 // for Json-LD 
    and Root = 
        | Root of Data list * Node      // the root object or array
    and Node = 
        | Object of Attribute list      // Object with properties
        | ObjectArray of Node list      // Array of Ent
        | LeafArray of Leaf list        // Array of Scalar
        | Attribute of string * Node
    and Leaf = 
        | Literal of Scalar
        
    [<Struct>]
    type NodeRef = 
        | NodeRef of string 
        | QName of string
    
        member this.GetResource (onto: OntologyGraph) : OntologyResource =
            match this with
            | NodeRef uri -> onto.CreateOntologyResource (onto.UriFactory.Create uri)
            | QName qname -> onto.CreateOntologyResource (onto.GetUriNode qname)
    
    type MappingGraph(mork:OntologyGraph, skos:SkosGraph) =
        do
            mork.UriFactory.InternUris <- true
            mork.NamespaceMap.AddNamespace("mork", Uri Prefixes.Mork)
            mork.NamespaceMap.AddNamespace("rdf", Uri Prefixes.Rdf)
            mork.NamespaceMap.AddNamespace("rdfs", Uri Prefixes.Rdfs)
            mork.NamespaceMap.AddNamespace("skos", Uri Prefixes.Skos)

        let RdfType = mork.CreateOntologyResource(mork.ResolveQName(Vocab.RdfType))
        
        // TODO: remove - just demonstrating the computation expression usage
        let ExprMorkClasses = 
            let collectMorkClasses (acc: Map<MorkConcept, OntologyResource * OntologyClass>) (owlClass: OntologyClass) =
                withUriNodes { 
                    let! uriNode = owlClass.Resource
                    let uri = uriNode.Uri.AbsolutePath
                    return uriNode
                } |> ignore
                acc
            in mork.OwlClasses |> Seq.fold collectMorkClasses Map.empty

        let MorkClasses =
            let collectClasses (acc: Map<MorkConcept, OntologyResource * OntologyClass>) (owlClass : OntologyClass) =
                match owlClass.Resource with
                    | :? IUriNode as uriNode ->
                        let uri = uriNode.Uri.AbsoluteUri
                        match ConceptTypes.TryGetValue uri with
                            | true, morkThing ->
                                let ref = NodeRef uri
                                let res = ref.GetResource mork
                                // printfn $"Storing Uri/NodeRef/Thing : {uri}/{res}/{morkThing}"
                                acc.Add(morkThing, (res, owlClass))
                            | _ -> acc
                    | _ -> (* anonymous classes / equivalent classes / etc *) acc
            in mork.OwlClasses |> Seq.fold collectClasses Map.empty
        
        let MorkSchemes : Map<MorkConcept, OntologyResource> =
            let schemes = [RepresentationScheme; TaxonomyScheme; MappingScheme; OntologicalScheme] in
            schemes |> Seq.fold (fun acc mc -> acc.Add(mc, mc.GetResource mork)) Map.empty

        member private this.debug (msg: string) (context: OntologyResource) =
            logger.debug(
                Log.setMessage msg >> 
                Log.addContextDestructured "Context" { Graph=mork; Skos=skos; Context=context }
            )

        // TODO: drop this once we have some tests in place...
        member this.print() = 
            let nodeId(n: INode) : Uri =
                    match n with
                        | :? IUriNode as uriNode ->
                            uriNode.Uri
                        | _ -> null
        
            in mork.Nodes
                |> Seq.filter (fun n -> n.NodeType.Equals VDS.RDF.NodeType.Uri)
                |> Seq.iter (fun (n: INode) -> printfn $"Node-{nodeId n}; {mork.GetTriplesWithSubject n}")

        member this.GetRepScheme : OntologyResource = this.GetScheme RepresentationScheme
        
        member this.GetTaxonomyScheme : OntologyResource = this.GetScheme TaxonomyScheme
        
        member this.GetMappingScheme : OntologyResource = this.GetScheme MappingScheme

        member private this.GetScheme (concept: MorkConcept) : OntologyResource =
            match MorkSchemes.TryGetValue concept with
                | true, scheme -> scheme
                | false, _ -> failwithf "Scheme for %A not found" (concept, MorkSchemes, MorkClasses)

        member internal this.GetMappings() =
            match MorkClasses.TryGetValue DataMapping with
                | true, (res, cls) ->
                    this.debug $"Found node for {cls}" res
                    this.CollectIndividuals cls |> Seq.toList
                | false, _ -> failwithf "Mappings not found in %A" MorkClasses

        member private this.CollectIndividuals (start: OntologyClass) : seq<OntologyResource> =
            start.SubClasses 
                |> Seq.append [start] // Include the start class itself 
                |> Seq.fold (fun acc cls -> Seq.append acc cls.Instances) Seq.empty

    let loadMappingOntology (uri: Uri) : MappingGraph =
        let g = new OntologyGraph()
        
        let loader = new Loader()
        // TODO: bundle artefacts and use EmbeddedResourceLoader.Load(g, "Namespace.EmbeddedFile.n3, YourAssembly")
        loader.LoadGraph(g, Uri "file:///Users/t4/work/mork/spec/Mork.ttl")
        loader.LoadGraph(g, uri)
        
        // FileLoader.Load(g, args[0]);

        let skos = new SkosGraph(g)        
        
        // skos.Nodes |> Seq.iter (fun n -> printfn "Skos node: %A" skos.ConceptSchemes)
        
        MappingGraph(g, skos)

    let loadMappingOntologyEmbedded (uri: Uri) : MappingGraph =
        let g = new OntologyGraph()
        EmbeddedResourceLoader.Load(g, "MorkSharp.Mork.ttl, MorkSharp")
        let loader = new Loader()
        loader.LoadGraph(g, uri)
        
        let skos = new SkosGraph(g)

        MappingGraph(g, skos)
