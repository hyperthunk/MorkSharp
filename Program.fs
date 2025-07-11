namespace MorkSharp

open VDS.RDF

// open OWLSharp.Extensions.Mork.MorkVocabulary
// open OWLSharp.OntologyModel

module CommandLine =

    open System
    open VDS.RDF.Ontology
    open VDS.RDF.Skos
    open VDS.RDF.Parsing
    open MorkSharp.Graph
    open MorkSharp.Vocab.Prefixes

    [<EntryPoint>]
    let main args =

        printfn $"Processing: {args}"
        
        let mg = loadMappingOntology (Uri args[0])
        
        let repScheme = mg.GetRepScheme
        printfn "Loaded Mork Ontology with %A" repScheme.Resource
        
        printfn "PRINTING GRAPH NODES"
        mg.print()
        
        let mappings = mg.GetMappings()
        printfn "Loaded Mappings %A" mappings
        0
