namespace Mork

open Mork.Vocab
open FsLibLog
open FsLibLog.Types

module Graph =

    open System
    open VDS.RDF
    open VDS.RDF.Ontology
    open VDS.RDF.Skos

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

    type MorkConcept with
        
        member internal this.GetClass (mork:OntologyGraph) : INode =
            mork.OwlClasses // THIS IS WRONG!
                |> Seq.tryFind (fun c -> c.Resource.NodeType.Equals NodeType.Uri)
                |> function
                    | Some cls -> cls.Resource
                    | None -> failwithf "Class not found for concept" 
    
    type MappingGraph(mork:OntologyGraph, skos:SkosGraph) =
        do
            mork.UriFactory.InternUris <- true
            mork.NamespaceMap.AddNamespace("mork", Uri Prefixes.Mork)
            mork.NamespaceMap.AddNamespace("rdf", Uri Prefixes.Rdf)
            mork.NamespaceMap.AddNamespace("rdfs", Uri Prefixes.Rdfs)
            // mork.NamespaceMap.AddNamespace("skos", Uri Prefixes.Skos)

        let RdfType = mork.CreateOntologyResource(mork.ResolveQName(Vocab.RdfType))
        
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
                    printfn $"Found node {res} for {cls}"
                    // cls.TriplesWithObject |> Seq.toList
                    
                    this.CollectIndividuals cls |> Seq.toList
                        
                    // mork.Triples.WithPredicateObject(RdfType.Resource, cls.Resource)
                    //|> Seq.map (fun t -> mork.CreateOntologyResource t.Subject)
                | false, _ -> failwithf "Mappings not found in %A" MorkClasses

        member private this.CollectIndividuals (start: OntologyClass) : seq<OntologyResource> =
            start.SubClasses 
                |> Seq.append [start] // Include the start class itself 
                |> Seq.fold (fun acc cls -> Seq.append acc cls.Instances) Seq.empty