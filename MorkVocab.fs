namespace rec Mork

open System
open VDS.RDF
open VDS.RDF.Ontology

module internal Vocab = 

    module Prefixes =
        [<Literal>]
        let Mork = "http://www.nebularis.org/ontologies/Mork#"
        
        [<Literal>]
        let Skos = "http://www.w3.org/2004/02/skos/core#"
        
        [<Literal>]
        let Dcterms = "http://purl.org/dc/terms/"
        
        [<Literal>]
        let Rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        
        [<Literal>]
        let Rdfs = "http://www.w3.org/2000/01/rdf-schema#"
        
        [<Literal>]
        let Owl = "http://www.w3.org/2002/07/owl#"

    [<Literal>]        
    let PrefLabel = "prefLabel"
    [<Literal>]
    let AltLabel = "altLabel"
    [<Literal>]
    let Note = "note"
    [<Literal>]
    let MorkMappingNote = "mappingNote"
    [<Literal>]
    let UserHasDeclined = "userDeclined" 
    [<Literal>]
    let SeeAlso = "seeAlso"
    [<Literal>]
    let Label = "label"
    [<Literal>]
    let Comment = "comment"
    [<Literal>]
    let IsDefinedBy = "isDefinedBy"
    
    let RdfType = "rdf:type"

    type MorkAnnotation =
        | MappingNote of string
        | UserDeclined of string
        | SkosPrefLabel of string
        | SkosAltLabel of string
        | SkosNote of string
        | RdfsSeeAlso of string
        | RdfsLabel of string
        | RdfsComment of string
        | RdfsIsDefinedBy of string
        | CustomAnnotation of string * string

    let MorkAnnotations = dict [
        Prefixes.Mork + MorkMappingNote, MappingNote
        Prefixes.Mork + UserHasDeclined, UserDeclined
        Prefixes.Skos + PrefLabel, SkosPrefLabel
        Prefixes.Skos + AltLabel, SkosAltLabel
        Prefixes.Skos + Note, SkosNote
        Prefixes.Rdfs + SeeAlso, RdfsSeeAlso
        Prefixes.Rdfs + Label, RdfsLabel
        Prefixes.Rdfs + Comment, RdfsComment
        Prefixes.Rdfs + IsDefinedBy, RdfsIsDefinedBy
    ]

    type NamedConcept =
        abstract member QName : string

    let FQName (axiom:NamedConcept) = Prefixes.Mork + axiom.QName

    [<Struct>]
    type MorkConcept =
        | DataConcept
        | Representation
        | DataMapping
        | MappingScheme
        | TaxonomyScheme
        | RepresentationScheme
        | SerializationFormat
        | OntologicalScheme
        | Digraph
        | Collection
        | CollectionElement
        | Lookup
        | Other of string

        interface NamedConcept with 
            member this.QName = 
                match this with
                    | DataConcept -> "DataConcept"
                    | Representation -> "Representation"
                    | DataMapping -> "DataMapping"
                    | MappingScheme -> "MappingScheme"
                    | TaxonomyScheme -> "TaxonomyScheme"
                    | RepresentationScheme -> "RepresentationScheme"
                    | SerializationFormat -> "SerializationFormat"
                    | OntologicalScheme -> "OntologicalScheme"
                    | Digraph -> "Digraph"
                    | Collection -> "Collection"
                    | CollectionElement -> "CollectionElement"
                    | Lookup -> "Lookup"
                    | Other s -> s  
        
        member this.GetResource (ont: OntologyGraph) : OntologyResource =
            ont.ResolveQName("mork:" + (this :> NamedConcept).QName) |> ont.CreateOntologyResource
    
    
    let ConceptTypes =
        Map [
            Prefixes.Mork + "DataConcept", DataConcept
            Prefixes.Mork + "Representation", Representation
            Prefixes.Mork + "DataMapping", DataMapping
            Prefixes.Mork + "MappingScheme", MappingScheme
            Prefixes.Mork + "TaxonomyScheme", TaxonomyScheme
            Prefixes.Mork + "RepresentationScheme", RepresentationScheme
            Prefixes.Mork + "SerializationFormat", SerializationFormat
            Prefixes.Mork + "OntologicalScheme", OntologicalScheme
            Prefixes.Mork + "Digraph", Digraph
            Prefixes.Mork + "Collection", Collection
            Prefixes.Mork + "CollectionElement", CollectionElement
            Prefixes.Mork + "Lookup", Lookup
        ]
 
    let classifyType (uri:string) =
        match ConceptTypes.TryGetValue uri with
        | true, thing -> thing
        | false, _    -> Other uri

    [<Literal>]
    let private KeywordBroad = "broad"
    [<Literal>]
    let private KeywordNarrow = "narrow"
    [<Literal>]
    let private KeywordExact = "exact"
    [<Literal>]
    let private KeywordCategory = "Category"
    [<Literal>]
    let private KeywordMatch = "Match"
    [<Literal>]
    let private KeywordIntransitiveExactMatch = "intransitiveExactMatch"
    [<Literal>]
    let private KeywordTBox = "TBox"
    [<Literal>]
    let private KeywordABox = "ABox"
    [<Literal>]
    let private KeywordRBox = "RBox"
        
    [<Struct>]
    type MatchType = 
        | Broad of Box 
        | Narrow of Box
        | Exact of Box
        | IntransitiveExact

        interface NamedConcept with 
            member this.QName
                with get() = 
                    match this with
                    | Broad box -> 
                        $"{KeywordBroad}{(box :> NamedConcept).QName}{KeywordCategory}{KeywordMatch}"
                    | Narrow box -> 
                        $"{KeywordNarrow}{(box :> NamedConcept).QName}{KeywordCategory}{KeywordMatch}"
                    | Exact box -> 
                        $"{KeywordExact}{(box :> NamedConcept).QName}{KeywordMatch}"
                    | IntransitiveExact -> KeywordIntransitiveExactMatch

    [<Struct>]
    type Box = 
        | TBox
        | ABox
        | RBox
        | Category

        interface NamedConcept with 
            member this.QName
                with get() = 
                    match this with
                    | TBox -> KeywordTBox
                    | ABox -> KeywordABox
                    | RBox -> KeywordRBox
                    | Category -> ""

    let BroadCategoryMatch = Broad Category
    let BroadTBoxCategoryMatch = Broad TBox
    let BroadABoxCategoryMatch = Broad ABox
    let BroadRBoxCategoryMatch = Broad RBox
    let NarrowCategoryMatch = Narrow Category
    let NarrowTBoxCategoryMatch = Narrow TBox
    let NarrowABoxCategoryMatch = Narrow ABox
    let NarrowRBoxCategoryMatch = Narrow RBox
    let IntransitiveExactMatch = IntransitiveExact
    let ExactTBoxMatch = Exact TBox
    let ExactABoxMatch = Exact ABox
    let ExactRBoxMatch = Exact RBox
    