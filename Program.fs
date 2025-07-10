namespace Mork

open Mork.Vocab
open VDS.RDF

// open OWLSharp.Extensions.Mork.MorkVocabulary
// open OWLSharp.OntologyModel

module CommandLine =

    open System
    open VDS.RDF.Ontology
    open VDS.RDF.Skos
    open VDS.RDF.Parsing
    open Mork.Graph
    open Mork.Vocab.Prefixes


    [<EntryPoint>]
    let main args =
        printfn $"Arguments passed to function : {args}"
        let g = new OntologyGraph()
        
        let loader = new Loader()
        loader.LoadGraph(g, Uri "file:///Users/t4/work/mork/spec/Mork.ttl")
        loader.LoadGraph(g, Uri args[0])
        
        // FileLoader.Load(g, args[0]);

        let skos = new SkosGraph(g)        
        
        // skos.Nodes |> Seq.iter (fun n -> printfn "Skos node: %A" skos.ConceptSchemes)
        
        let mg = MappingGraph(g, skos)
        let repScheme = mg.GetRepScheme
        printfn "Loaded Mork Ontology with %A" repScheme.Resource
        
        let nodeId(n: INode) : Uri =
            match n with
                | :? IUriNode as uriNode ->
                    uriNode.Uri
                | _ -> null
        
        printfn "PRINTING GRAPH NODES"
        g.Nodes
            |> Seq.filter (fun n -> n.NodeType.Equals VDS.RDF.NodeType.Uri)
            |> Seq.iter (fun (n: INode) -> printfn $"Node-{nodeId n}; {g.GetTriplesWithSubject n}")
        
        let mappings = mg.GetMappings()
        printfn "Loaded Mappings %A" mappings
        0
