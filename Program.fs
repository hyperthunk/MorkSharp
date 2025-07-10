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
    open MorkSharp.TypeProviders


    [<EntryPoint>]
    let main args =

        // Get a specific property
        let p = MorkProperty.BroadCategoryMatch
        printfn "Name: %s, IRI: %s" p.Name p.Iri

        // Enumerate all properties
        for prop in MorkProperty.All do
            printfn "Property: %s IRI: %s" prop.Name prop.Iri

        // Use a custom property IRI
        let custom = MorkProperty.CreateCustom("http://www.example.org/ont#myProp")
        printfn "Custom: %s %s" custom.Name custom.Iri

        printfn $"Processing: {args}"
        
        let mg = loadMappingOntology (Uri args[0])
        
        let repScheme = mg.GetRepScheme
        printfn "Loaded Mork Ontology with %A" repScheme.Resource
        
        printfn "PRINTING GRAPH NODES"
        mg.print()
        
        let mappings = mg.GetMappings()
        printfn "Loaded Mappings %A" mappings
        0
