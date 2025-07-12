namespace rec MorkSharp

open VDS.RDF
open VDS.RDF.Ontology

module internal Vocab = 

    module internal Constants = 
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
        let IsDefinedBy = "isDefinedBylet "
        [<Literal>]
        let KeywordBroad = "broad"
        [<Literal>]
        let KeywordNarrow = "narrow"
        [<Literal>]
        let KeywordExact = "exact"
        [<Literal>]
        let KeywordCategory = "Category"
        [<Literal>]
        let KeywordMatch = "Match"
        [<Literal>]
        let KeywordIntransitiveExactMatch = "intransitiveExactMatch"
        [<Literal>]
        let KeywordTBox = "TBox"
        [<Literal>]
        let KeywordABox = "ABox"
        [<Literal>]
        let KeywordRBox = "RBox"


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
    
    let RdfType = "rdf:type"


    type NamedConcept =
        abstract member QName : string
        abstract member FQName : string


    type MorkAnnotation =
        | MappingNote 
        | UserDeclined
        | SkosPrefLabel
        | SkosAltLabel
        | SkosNote
        | RdfsSeeAlso
        | RdfsLabel
        | RdfsComment
        | RdfsIsDefinedBy
        | CustomAnnotation of string * string

        interface NamedConcept with
            member this.QName =
                match this with
                | MappingNote -> Constants.MorkMappingNote
                | UserDeclined -> Constants.UserHasDeclined
                | SkosPrefLabel -> Constants.PrefLabel
                | SkosAltLabel -> Constants.AltLabel
                | SkosNote -> Constants.Note
                | RdfsSeeAlso -> Constants.SeeAlso
                | RdfsLabel -> Constants.Label
                | RdfsComment -> Constants.Comment
                | RdfsIsDefinedBy -> Constants.IsDefinedBy
                | CustomAnnotation (prefix, str) -> $"{prefix}#{str}"

            member this.FQName = Prefixes.Mork + (this :> NamedConcept).QName

    let MorkAnnotations = dict [
        Prefixes.Mork + Constants.MorkMappingNote, MappingNote
        Prefixes.Mork + Constants.UserHasDeclined, UserDeclined
        Prefixes.Skos + Constants.PrefLabel, SkosPrefLabel
        Prefixes.Skos + Constants.AltLabel, SkosAltLabel
        Prefixes.Skos + Constants.Note, SkosNote
        Prefixes.Rdfs + Constants.SeeAlso, RdfsSeeAlso
        Prefixes.Rdfs + Constants.Label, RdfsLabel
        Prefixes.Rdfs + Constants.Comment, RdfsComment
        Prefixes.Rdfs + Constants.IsDefinedBy, RdfsIsDefinedBy
    ]

    let FQName (axiom:NamedConcept) = Prefixes.Mork + axiom.QName
        
    type SemanticRel =
        | Associative of Box

    [<Struct>]
    type MatchType = 
        | Broad of Box 
        | Narrow of Box
        | Exact of Box
        | IntransitiveExact

        interface NamedConcept with 
            member this.QName
                with get() = 
                    let brd = Constants.KeywordBroad
                    let nrw = Constants.KeywordNarrow
                    let exa = Constants.KeywordExact
                    let cat = Constants.KeywordCategory
                    let mat = Constants.KeywordMatch
                    in match this with
                        | Broad box -> 
                            $"{brd}{(box :> NamedConcept).QName}{cat}{mat}"
                        | Narrow box -> 
                            $"{nrw}{(box :> NamedConcept).QName}{cat}{mat}"
                        | Exact box -> 
                            $"{exa}{(box :> NamedConcept).QName}{mat}"
                        | IntransitiveExact -> Constants.KeywordIntransitiveExactMatch

            member this.FQName = Prefixes.Mork + (this :> NamedConcept).QName

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
                    | TBox -> Constants.KeywordTBox
                    | ABox -> Constants.KeywordABox
                    | RBox -> Constants.KeywordRBox
                    | Category -> ""

            member this.FQName = (this :> NamedConcept).QName

    (* let BroadCategoryMatch = Broad Category
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
    let ExactRBoxMatch = Exact RBox *)
    
    [<Struct>]
    type MorkConcept =
        | DataConcept
        | Objectification
        | Representation
        | DataMapping
        | Datum
        | DeferredContext
        | Hypothesis
        | IndexedMapping
        | Lookup
        | UncertainMapping
        | WeightedMatch
        | MappingScheme
        | TaxonomyScheme
        | RepresentationScheme
        | OntologicalScheme
        | SerializationFormat
        | Digraph
        | Collection
        | CollectionElement
        | OrderedCollectionElement
        | SkosConcept
        | SkosConceptScheme
        | SkosOrderedCollection
        | Other of string

        interface NamedConcept with 
            member this.QName = 
                match this with
                    | DataConcept -> "DataConcept"
                    | Objectification -> "Objectification"
                    | Representation -> "Representation"
                    | DataMapping -> "DataMapping"
                    | Datum -> "Datum"
                    | DeferredContext -> "DeferredContext"
                    | Hypothesis -> "Hypothesis"
                    | IndexedMapping -> "IndexedMapping"
                    | Lookup -> "Lookup"
                    | UncertainMapping -> "UncertainMapping"
                    | WeightedMatch -> "WeightedMatch"
                    | MappingScheme -> "MappingScheme"
                    | TaxonomyScheme -> "TaxonomyScheme"
                    | RepresentationScheme -> "RepresentationScheme"
                    | SerializationFormat -> "SerializationFormat"
                    | OntologicalScheme -> "OntologicalScheme"
                    | Digraph -> "Digraph"
                    | Collection -> "Collection"
                    | CollectionElement -> "CollectionElement"
                    | OrderedCollectionElement -> "OrderedCollectionElement"
                    | SkosConcept -> "Concept"
                    | SkosConceptScheme -> "ConceptScheme"
                    | SkosOrderedCollection ->  "OrderedCollection"
                    | Other s -> s  
        
            member this.FQName = Prefixes.Mork + (this :> NamedConcept).QName

        member this.GetResource (ont: OntologyGraph) : OntologyResource =
            ont.ResolveQName("mork:" + (this :> NamedConcept).QName) |> ont.CreateOntologyResource
    
    
    let ConceptTypes =
        Map [
            Prefixes.Mork + "DataConcept", DataConcept
            Prefixes.Mork + "Objectification", Objectification
            Prefixes.Mork + "Representation", Representation
            Prefixes.Mork + "DataMapping", DataMapping
            Prefixes.Mork + "Datum", Datum
            Prefixes.Mork + "DeferredContext", DeferredContext
            Prefixes.Mork + "Hypothesis", Hypothesis
            Prefixes.Mork + "IndexedMapping", IndexedMapping
            Prefixes.Mork + "Lookup", Lookup
            Prefixes.Mork + "UncertainMapping", UncertainMapping
            Prefixes.Mork + "WeightedMatch", WeightedMatch
            Prefixes.Mork + "MappingScheme", MappingScheme
            Prefixes.Mork + "TaxonomyScheme", TaxonomyScheme
            Prefixes.Mork + "RepresentationScheme", RepresentationScheme
            Prefixes.Mork + "OntologicalScheme", OntologicalScheme
            Prefixes.Mork + "SerializationFormat", SerializationFormat
            Prefixes.Mork + "Digraph", Digraph
            Prefixes.Mork + "Collection", Collection
            Prefixes.Mork + "CollectionElement", CollectionElement
            Prefixes.Mork + "OrderedCollectionElement", OrderedCollectionElement
            Prefixes.Skos + "Concept", SkosConcept
            Prefixes.Skos + "ConceptScheme", SkosConceptScheme            
        ]
 
    let classifyType (uri:string) =
        match ConceptTypes.TryGetValue uri with
        | true, thing -> thing
        | false, _    -> Other uri

    type SemanticDistance = 
        | Broad         // the object is broader than the subject
        | Narrow        // the object is narrower than the subject
        | Close         // the object is close to the subject
        | Exact         // the object is exactly the same as the subject
        | NA            // the distance is not applicable to mapping outputs

    // the meaning of the relationship between the subject and object
    type PredicateSemantics = 
        | Associative   // the relationship between the subject and object is associative
        | Compositional // the relationship between the subject and object is compositional
        | Categorical   // the relationship between the subject and object is categorical
        | Dependent     // the subject is dependent on the object
        | Identity      // the object is an identity of the subject
        | Indicative    // the subject is indicative of the object
        | Configuration // the object is a configuration item for the subject
        | UpdateContext // the object updates the context of the subject  
        | NonApplicable // the semantics of the relationship are not applicable to mapping outputs

    // the meaning of the relationship between the subject and object
    type NodeSemantics = 
        | Related       // the object is related to the subject
        | Mapping       // the object is a mapping 
        | Template      // the object is a template for the subject
        | Hypotheses    // the object is a hypothesis for the subject
        | UnknownOrNA   // the semanties of the relationship are unknown or not applicable

    type ExecutionScope = 
        | Local         // applies to the current subject only
        | ContextLocal  // applies to the current context
        | Inherited     // what applies to the subject, applies to the object
        | Deferred      // applies to the deferredTo object 
        | Applicative   // applies the referenced object to the current subject

    type NodeAction =
        | Assert        // Assert the node in the current context
        | Traverse      // Traverse the node(s) in the current context
        | Yield         // Yield the node(s) in the current context
        | Collect       // Collect the node(s) in the current context

    type MorkNode =
        | ConceptNode of MorkConcept
    
    type DataNode =
        {
            Resource: OntologyResource
            Value: LiteralNode
        }
    
    type PropertyNode =
        {   
            Distance: SemanticDistance
            Predicate: PredicateSemantics
            Object: NodeSemantics
            Scope: ExecutionScope
        }

    type XsdType = 
        | XsdString
        | XsdInt

    type MorkProperty =
        | ObjectProperty of PropertyNode * string
        | LinkingProperty of string
        | SkosObjectProperty of PropertyNode * string
        | DataProperty of XsdType * string

        member this.GetResource (ont: OntologyGraph) : OntologyResource =
            match this with
            | ObjectProperty (_, iri) -> ont.ResolveQName (Prefixes.Mork + iri) |> ont.CreateOntologyResource
            | LinkingProperty iri -> ont.ResolveQName (Prefixes.Mork + iri) |> ont.CreateOntologyResource
            | SkosObjectProperty (_, iri) -> ont.ResolveQName (Prefixes.Skos + iri) |> ont.CreateOntologyResource
            | DataProperty (_, iri) -> ont.ResolveQName (Prefixes.Mork + iri) |> ont.CreateOntologyResource

        interface NamedConcept with
            member this.QName: string = 
                match this with 
                | ObjectProperty (_, iri) -> iri
                | LinkingProperty iri -> iri
                | SkosObjectProperty (_, iri) -> iri
                | DataProperty (_, iri) -> iri

            member this.FQName: string =
                match this with 
                | ObjectProperty (_, _) -> Prefixes.Mork + (this :> NamedConcept).QName
                | LinkingProperty _ -> Prefixes.Mork + (this :> NamedConcept).QName
                | SkosObjectProperty (_, _) -> Prefixes.Skos + (this :> NamedConcept).QName
                | DataProperty (_, _) -> Prefixes.Mork + (this :> NamedConcept).QName

        member this.GetExecutionScope: ExecutionScope =
            match this with
            | ObjectProperty (node, _) -> node.Scope
            | LinkingProperty _ -> ContextLocal // Linking properties are always context-local
            | SkosObjectProperty (node, _) -> node.Scope
            | DataProperty (_, _) -> Local // Data properties are always local

        member this.GetSemanticDistance: option<SemanticDistance> =
            match this with 
            | ObjectProperty (node, _) -> Some node.Distance
            | SkosObjectProperty (node, _) -> Some node.Distance
            | LinkingProperty _ -> None 
            | DataProperty (_, _) -> None

        member this.GetPredicateSemantics: option<PredicateSemantics> =
            let linkingSemantics propName = 
                match propName with
                    | "format" -> Some Configuration
                    | "relatedProperty" -> Some Configuration
                    | "siblingMapping" -> Some Associative
                    | "hasConcept" -> Some Associative
                    | "memberOf" -> Some Associative
                    | _ -> Some NonApplicable
            match this with 
            | ObjectProperty (node, _) -> Some node.Predicate
            | LinkingProperty propName -> linkingSemantics propName
            | SkosObjectProperty (node, _) -> Some node.Predicate
            | DataProperty (_, _) -> None

        member this.GetNodeSemantics: option<NodeSemantics> =
            match this with 
            | ObjectProperty (node, _) -> Some node.Object
            | SkosObjectProperty (node, _) -> Some node.Object
            | LinkingProperty _ -> None
            | DataProperty (_, _) -> None

    module Skos =
        open VDS.RDF.Ontology
        open MorkSharp.Vocab

        let MorkUri = Prefixes.Mork
        let SkosUri = Prefixes.Skos

        module Properties =

            let private defaultConfig = { Distance = NA; Predicate = NonApplicable; Object = Related; Scope = Local }

            let HasTopConcept = SkosObjectProperty (defaultConfig, "hasTopConcept")

            let InScheme = 
                SkosObjectProperty ({ defaultConfig with Predicate = Associative }, "inScheme")

            let TopConceptOf = 
                SkosObjectProperty ({ defaultConfig with Predicate = Associative }, "topConceptOf")

            let Member = 
                SkosObjectProperty ({ defaultConfig with Predicate = Compositional }, "member")
            
            let MemberList = 
                SkosObjectProperty ({ defaultConfig with Distance = Narrow; 
                                                         Predicate = Associative }, "memberList")

            let Broader = 
                SkosObjectProperty ({ defaultConfig with Distance = Broad; 
                                                         Predicate = Associative }, "broader")

            let Narrower = 
                SkosObjectProperty ({ defaultConfig with Distance = Narrow; 
                                                         Predicate = Associative }, "narrower")

            let MappingRelation = 
                SkosObjectProperty ({ defaultConfig with Distance = NA; 
                                                         Predicate = Associative;
                                                         Object = Mapping }, "mappingRelation")

            let BroadMatch = 
                SkosObjectProperty ({ defaultConfig with Distance = Broad; 
                                                         Predicate = Indicative;
                                                         Object = Mapping }, "broadMatch")

            let NarrowMatch = 
                SkosObjectProperty ({ defaultConfig with Distance = Narrow; 
                                                         Predicate = Indicative;
                                                         Object = Mapping }, "narrowMatch")
            
            let ExactMatch = 
                SkosObjectProperty ({ defaultConfig with Distance = Exact; 
                                                         Predicate = Indicative;
                                                         Object = Mapping }, "exactMatch")

            let CloseMatch = 
                SkosObjectProperty ({ defaultConfig with Distance = Close; 
                                                         Predicate = Indicative;
                                                         Object = Mapping }, "closeMatch")

            let SkosRelated = 
                SkosObjectProperty ({ defaultConfig with Distance = NA; 
                                                         Predicate = Associative;
                                                         Object = Related }, "related")

            let RelatedMatch = 
                SkosObjectProperty ({ defaultConfig with Distance = NA; 
                                                         Predicate = Associative;
                                                         Object = Mapping }, "relatedMatch")

            let ObjectProperties : Map<string, MorkProperty> = 
                [
                    HasTopConcept;
                    InScheme;
                    TopConceptOf;
                    Member;
                    MemberList;
                    Broader;
                    Narrower;
                    MappingRelation;
                    BroadMatch;
                    NarrowMatch;
                    ExactMatch;
                    CloseMatch;
                    SkosRelated;
                    RelatedMatch;
                ] |> List.map (fun prop -> (prop :> NamedConcept).FQName, prop) |> Map.ofList

    // --- End of MorkProperty model ---