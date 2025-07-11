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
        
            member this.FQName = Prefixes.Mork + (this :> NamedConcept).QName

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

    type SemanticDistance = 
        | Broad         // the object is broader than the subject
        | Narrow        // the object is narrower than the subject
        | Close         // the object is close to the subject
        | Exact         // the object is exactly the same as the subject

    // the meaning of the relationship between the subject and object
    type PredicateSemantics = 
        | Associative   // the relationship between the subject and object is associative
        | Composite     // the relationship between the subject and object is compositional
        | Categorical   // the relationship between the subject and object is categorical
        | Dependent     // the subject is dependent on the object
        | Identity      // the object is an identity of the subject
        | Indicative    // the object is indicative of the subject

    // the meaning of the relationship between the subject and object
    type NodeSemantics = 
        | Related       // the object is related to the subject
        | Mapping       // the object is a mapping 
        | Template      // the object is a template for the subject
        | Hypothesis    // the object is a hypothesis for the subject

    type ExecutionScope = 
        | Local         // applies to the current subject only
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
        | SkosObjectProperty of PropertyNode * string
        | DataProperty of XsdType * string

        member this.GetResource (ont: OntologyGraph) : OntologyResource =
            match this with
            | ObjectProperty (_, iri) -> ont.ResolveQName (Prefixes.Mork + iri) |> ont.CreateOntologyResource
            | SkosObjectProperty (_, iri) -> ont.ResolveQName (Prefixes.Skos + iri) |> ont.CreateOntologyResource
            | DataProperty (_, iri) -> ont.ResolveQName (Prefixes.Mork + iri) |> ont.CreateOntologyResource

        interface NamedConcept with
            member this.QName: string = 
                match this with 
                | ObjectProperty (_, iri) -> iri
                | SkosObjectProperty (_, iri) -> iri
                | DataProperty (_, iri) -> iri

            member this.FQName: string =
                match this with 
                | ObjectProperty (_, _) -> Prefixes.Mork + (this :> NamedConcept).QName
                | SkosObjectProperty (_, _) -> Prefixes.Skos + (this :> NamedConcept).QName
                | DataProperty (_, _) -> Prefixes.Mork + (this :> NamedConcept).QName

        member this.GetExecutionScope: ExecutionScope =
            match this with
            | ObjectProperty (node, _) -> node.Scope
            | SkosObjectProperty (node, _) -> node.Scope
            | DataProperty (_, _) -> Local // Data properties are always local

        member this.GetSemanticDistance: option<SemanticDistance> =
            match this with 
            | ObjectProperty (node, _) -> Some node.Distance
            | SkosObjectProperty (node, _) -> Some node.Distance
            | DataProperty (_, _) -> None

        member this.GetPredicateSemantics: option<PredicateSemantics> =
            match this with 
            | ObjectProperty (node, _) -> Some node.Predicate
            | SkosObjectProperty (node, _) -> Some node.Predicate
            | DataProperty (_, _) -> None

        member this.GetNodeSemantics: option<NodeSemantics> =
            match this with 
            | ObjectProperty (node, _) -> Some node.Object
            | SkosObjectProperty (node, _) -> Some node.Object
            | DataProperty (_, _) -> None

    let associative = 
        { Distance = Narrow
          Predicate = Associative
          Object = Related
          Scope = Local }

    let composite = 
        { Distance = Narrow
          Predicate = Composite
          Object = Related
          Scope = Local }

    let categorical = 
        { Distance = Narrow
          Predicate = Categorical
          Object = Related
          Scope = Local }

    let BroadCategoryMatch = ObjectProperty ({ categorical with Distance=Broad }, "broadCategoryMatch")
    let NarrowCategoryMatch = ObjectProperty (categorical, "narrowCategoryMatch")

    // concept roles
    let BroadConceptRole = 
        ObjectProperty ({ associative with Distance=Broad }, "broadConceptRole")
    let BroadNavigableConceptRole = 
        ObjectProperty ({ associative with Distance=Broad }, "broadNavigableConceptRole")
    let NarrowConceptRole = 
        ObjectProperty (associative, "narrowConceptRole")    
    let NarrowNavigableConceptRole = 
        ObjectProperty (associative, "narrowNavigableConceptRole")

    let AssociativeBroader = 
        ObjectProperty ({ associative with Distance=Broad }, "associativeBroader")    
    let AssociativeNarrower = ObjectProperty (associative, "associativeBroader")
    
    let CompositeBroader =
        ObjectProperty ({ composite with Distance=Broad }, "compositeBroader")
    let CompositeNarrower = ObjectProperty (composite, "compositeNarrower")

    let ex (op: MorkProperty) =
        match op with 
            | ObjectProperty (_, iri) -> Prefixes.Mork + iri
            | SkosObjectProperty (_, iri) -> Prefixes.Skos + iri
            | DataProperty (_, iri) -> Prefixes.Mork + iri

    (*
    /// Encodes all Mork object properties for lookup and pattern matching
    [<Struct>]
    type MorkPropertyX =
        // Hierarchy for all known object properties
        | AssertsPropertyDomains
        | AssertsPropertyRanges
        | AssociativeBroader        [x]
        | AssociativeNarrower       [x]
        | BroadABoxCategoryMatch
        | BroadCategoryMatch        [x]
        | BroadConceptRole          [x]
        | BroadNavigableConceptRole [x]
        | BroadRBoxCategoryMatch
        | BroadTBoxCategoryMatch
        | BroaderApplicative
        | BroaderMapping
        | CloseMapping
        | CompositeBroader          [x]
        | CompositeBroaderMapping
        | CompositeBroaderTemplate
        | CompositeNarrower         [x]
        | CompositeNarrowerMapping
        | CompositeNarrowerTemplate
        | Concatenation
        | ConceptScheme
        | DeferredMapping
        | DependentMapping
        | DigraphMatch
        | DigraphOf
        | ElementType
        | ExactABoxMatch
        | ExactMapping
        | ExactRBoxMatch
        | ExactTBoxMatch
        | Format
        | HasConcept
        | HasMapping
        | HypothesisMapping
        | IdentityTemplateMapping
        | IndicativeMapping
        | InterpolationComponent
        | IntransitiveExactMatch
        | InverseRBoxMatch
        | LexicalMatch
        | MappingFor
        | HasMappingScheme
        | MemberOf
        | MemberProperty
        | MissingOrUnrelatedMatch
        | NarrowABoxCategoryMatch
        | NarrowCategoryMatch           [x]
        | NarrowConceptRole             [x]
        | NarrowNavigableConceptRole    [x]
        | NarrowRBoxCategoryMatch
        | NarrowTBoxCategoryMatch
        | NarrowerMapping
        | HasOntologicalScheme
        | PartialMatch
        | PathMatch
        | PlaceholderMapping
        | PossibleMatch
        | PropertyMappingAssertions
        | ReferenceDataMapping
        | ReferenceDataPath
        | RelatedMapping
        | RelatedProperty
        | RepresentationOf
        | HasRepresentationScheme
        | RepresentedAs
        | SemanticMatch
        | StructuralMatch
        | TemplateMapping
        | WeightedBroader
        | WeightedNarrower
        | YieldConcept
        | CustomProperty of string

        interface NamedConcept with
            member this.QName =
                match this with
                | AssertsPropertyDomains -> "assertsPropertyDomains"
                | AssertsPropertyRanges -> "assertsPropertyRanges"
                | AssociativeBroader -> "associativeBroader"
                | AssociativeNarrower -> "associativeNarrower"
                | BroadABoxCategoryMatch -> "broadABoxCategoryMatch"
                | BroadCategoryMatch -> "broadCategoryMatch"
                | BroadConceptRole -> "broadConceptRole"
                | BroadNavigableConceptRole -> "broadNavigableConceptRole"
                | BroadRBoxCategoryMatch -> "broadRBoxCategoryMatch"
                | BroadTBoxCategoryMatch -> "broadTBoxCategoryMatch"
                | BroaderApplicative -> "broaderApplicative"
                | BroaderMapping -> "broaderMapping"
                | CloseMapping -> "closeMapping"
                | CompositeBroader -> "compositeBroader"
                | CompositeBroaderMapping -> "compositeBroaderMapping"
                | CompositeBroaderTemplate -> "compositeBroaderTemplate"
                | CompositeNarrower -> "compositeNarrower"
                | CompositeNarrowerMapping -> "compositeNarrowerMapping"
                | CompositeNarrowerTemplate -> "compositeNarrowerTemplate"
                | Concatenation -> "concatenation"
                | ConceptScheme -> "conceptScheme"
                | DeferredMapping -> "deferredMapping"
                | DependentMapping -> "dependentMapping"
                | DigraphMatch -> "digraphMatch"
                | DigraphOf -> "digraphOf"
                | ElementType -> "elementType"
                | ExactABoxMatch -> "exactABoxMatch"
                | ExactMapping -> "exactMapping"
                | ExactRBoxMatch -> "exactRBoxMatch"
                | ExactTBoxMatch -> "exactTBoxMatch"
                | Format -> "format"
                | HasConcept -> "hasConcept"
                | HasMapping -> "hasMapping"
                | HypothesisMapping -> "hypothesisMapping"
                | IdentityTemplateMapping -> "identityTemplateMapping"
                | IndicativeMapping -> "indicativeMapping"
                | InterpolationComponent -> "interpolationComponent"
                | IntransitiveExactMatch -> "intransitiveExactMatch"
                | InverseRBoxMatch -> "inverseRBoxMatch"
                | LexicalMatch -> "lexicalMatch"
                | MappingFor -> "mappingFor"
                | HasMappingScheme -> "mappingScheme"
                | MemberOf -> "memberOf"
                | MemberProperty -> "memberProperty"
                | MissingOrUnrelatedMatch -> "missingOrUnrelatedMatch"
                | NarrowABoxCategoryMatch -> "narrowABoxCategoryMatch"
                | NarrowCategoryMatch -> "narrowCategoryMatch"
                | NarrowConceptRole -> "narrowConceptRole"
                | NarrowNavigableConceptRole -> "narrowNavigableConceptRole"
                | NarrowRBoxCategoryMatch -> "narrowRBoxCategoryMatch"
                | NarrowTBoxCategoryMatch -> "narrowTBoxCategoryMatch"
                | NarrowerMapping -> "narrowerMapping"
                | HasOntologicalScheme -> "ontologicalScheme"
                | PartialMatch -> "partialMatch"
                | PathMatch -> "pathMatch"
                | PlaceholderMapping -> "placeholderMapping"
                | PossibleMatch -> "possibleMatch"
                | PropertyMappingAssertions -> "propertyMappingAssertions"
                | ReferenceDataMapping -> "referenceDataMapping"
                | ReferenceDataPath -> "referenceDataPath"
                | RelatedMapping -> "relatedMapping"
                | RelatedProperty -> "relatedProperty"
                | RepresentationOf -> "representationOf"
                | HasRepresentationScheme -> "representationScheme"
                | RepresentedAs -> "representedAs"
                | SemanticMatch -> "semanticMatch"
                | StructuralMatch -> "structuralMatch"
                | TemplateMapping -> "templateMapping"
                | WeightedBroader -> "weightedBroader"
                | WeightedNarrower -> "weightedNarrower"
                | YieldConcept -> "yieldConcept"
                | CustomProperty s -> s

            member this.FQName = Prefixes.Mork + (this :> NamedConcept).QName

        member this.GetResource (ont:OntologyGraph) : OntologyResource =
            let qname =
                match this with
                | CustomProperty iri when iri.StartsWith("http") -> iri
                | _ -> Prefixes.Mork + (this :> NamedConcept).QName
            ont.CreateOntologyResource (ont.ResolveQName qname)

    // Map from fully qualified IRI to MorkProperty
    let MorkProperties : Map<string, MorkPropertyX> =
        [
            Prefixes.Mork + "assertsPropertyDomains", AssertsPropertyDomains
            Prefixes.Mork + "assertsPropertyRanges", AssertsPropertyRanges
            Prefixes.Mork + "associativeBroader", AssociativeBroader
            Prefixes.Mork + "associativeNarrower", AssociativeNarrower
            Prefixes.Mork + "broadABoxCategoryMatch", BroadABoxCategoryMatch
            Prefixes.Mork + "broadCategoryMatch", BroadCategoryMatch
            Prefixes.Mork + "broadConceptRole", BroadConceptRole
            Prefixes.Mork + "broadNavigableConceptRole", BroadNavigableConceptRole
            Prefixes.Mork + "broadRBoxCategoryMatch", BroadRBoxCategoryMatch
            Prefixes.Mork + "broadTBoxCategoryMatch", BroadTBoxCategoryMatch
            Prefixes.Mork + "broaderApplicative", BroaderApplicative
            Prefixes.Mork + "broaderMapping", BroaderMapping
            Prefixes.Mork + "closeMapping", CloseMapping
            Prefixes.Mork + "compositeBroader", CompositeBroader
            Prefixes.Mork + "compositeBroaderMapping", CompositeBroaderMapping
            Prefixes.Mork + "compositeBroaderTemplate", CompositeBroaderTemplate
            Prefixes.Mork + "compositeNarrower", CompositeNarrower
            Prefixes.Mork + "compositeNarrowerMapping", CompositeNarrowerMapping
            Prefixes.Mork + "compositeNarrowerTemplate", CompositeNarrowerTemplate
            Prefixes.Mork + "concatenation", Concatenation
            Prefixes.Mork + "conceptScheme", ConceptScheme
            Prefixes.Mork + "deferredMapping", DeferredMapping
            Prefixes.Mork + "dependentMapping", DependentMapping
            Prefixes.Mork + "digraphMatch", DigraphMatch
            Prefixes.Mork + "digraphOf", DigraphOf
            Prefixes.Mork + "elementType", ElementType
            Prefixes.Mork + "exactABoxMatch", ExactABoxMatch
            Prefixes.Mork + "exactMapping", ExactMapping
            Prefixes.Mork + "exactRBoxMatch", ExactRBoxMatch
            Prefixes.Mork + "exactTBoxMatch", ExactTBoxMatch
            Prefixes.Mork + "format", Format
            Prefixes.Mork + "hasConcept", HasConcept
            Prefixes.Mork + "hasMapping", HasMapping
            Prefixes.Mork + "hypothesisMapping", HypothesisMapping
            Prefixes.Mork + "identityTemplateMapping", IdentityTemplateMapping
            Prefixes.Mork + "indicativeMapping", IndicativeMapping
            Prefixes.Mork + "interpolationComponent", InterpolationComponent
            Prefixes.Mork + "intransitiveExactMatch", IntransitiveExactMatch
            Prefixes.Mork + "inverseRBoxMatch", InverseRBoxMatch
            Prefixes.Mork + "lexicalMatch", LexicalMatch
            Prefixes.Mork + "mappingFor", MappingFor
            Prefixes.Mork + "mappingScheme", HasMappingScheme
            Prefixes.Mork + "memberOf", MemberOf
            Prefixes.Mork + "memberProperty", MemberProperty
            Prefixes.Mork + "missingOrUnrelatedMatch", MissingOrUnrelatedMatch
            Prefixes.Mork + "narrowABoxCategoryMatch", NarrowABoxCategoryMatch
            Prefixes.Mork + "narrowCategoryMatch", NarrowCategoryMatch
            Prefixes.Mork + "narrowConceptRole", NarrowConceptRole
            Prefixes.Mork + "narrowNavigableConceptRole", NarrowNavigableConceptRole
            Prefixes.Mork + "narrowRBoxCategoryMatch", NarrowRBoxCategoryMatch
            Prefixes.Mork + "narrowTBoxCategoryMatch", NarrowTBoxCategoryMatch
            Prefixes.Mork + "narrowerMapping", NarrowerMapping
            Prefixes.Mork + "ontologicalScheme", HasOntologicalScheme
            Prefixes.Mork + "partialMatch", PartialMatch
            Prefixes.Mork + "pathMatch", PathMatch
            Prefixes.Mork + "placeholderMapping", PlaceholderMapping
            Prefixes.Mork + "possibleMatch", PossibleMatch
            Prefixes.Mork + "propertyMappingAssertions", PropertyMappingAssertions
            Prefixes.Mork + "referenceDataMapping", ReferenceDataMapping
            Prefixes.Mork + "referenceDataPath", ReferenceDataPath
            Prefixes.Mork + "relatedMapping", RelatedMapping
            Prefixes.Mork + "relatedProperty", RelatedProperty
            Prefixes.Mork + "representationOf", RepresentationOf
            Prefixes.Mork + "representationScheme", HasRepresentationScheme
            Prefixes.Mork + "representedAs", RepresentedAs
            Prefixes.Mork + "semanticMatch", SemanticMatch
            Prefixes.Mork + "structuralMatch", StructuralMatch
            Prefixes.Mork + "templateMapping", TemplateMapping
            Prefixes.Mork + "weightedBroader", WeightedBroader
            Prefixes.Mork + "weightedNarrower", WeightedNarrower
            Prefixes.Mork + "yieldConcept", YieldConcept

            // SKOS and RDF(S) properties used in the ontology but not Mork namespace
            Prefixes.Skos + "broader", CustomProperty (Prefixes.Skos + "broader")
            Prefixes.Skos + "narrower", CustomProperty (Prefixes.Skos + "narrower")
            Prefixes.Skos + "related", CustomProperty (Prefixes.Skos + "related")
            Prefixes.Skos + "broaderMatch", CustomProperty (Prefixes.Skos + "broaderMatch")
            Prefixes.Skos + "narrowMatch", CustomProperty (Prefixes.Skos + "narrowMatch")
            Prefixes.Skos + "closeMatch", CustomProperty (Prefixes.Skos + "closeMatch")
            Prefixes.Skos + "exactMatch", CustomProperty (Prefixes.Skos + "exactMatch")
            Prefixes.Skos + "mappingRelation", CustomProperty (Prefixes.Skos + "mappingRelation")
            Prefixes.Skos + "inScheme", CustomProperty (Prefixes.Skos + "inScheme")
            Prefixes.Skos + "OrderedCollection", CustomProperty (Prefixes.Skos + "OrderedCollection")
            Prefixes.Rdfs + "seeAlso", CustomProperty (Prefixes.Rdfs + "seeAlso")
            Prefixes.Rdfs + "subPropertyOf", CustomProperty (Prefixes.Rdfs + "subPropertyOf")
            Prefixes.Owl + "inverseOf", CustomProperty (Prefixes.Owl + "inverseOf")
            Prefixes.Owl + "deprecated", CustomProperty (Prefixes.Owl + "deprecated")
        ]
        |> Map.ofList

    let classifyProperty (iri:string) : MorkPropertyX =
        match MorkProperties.TryFind iri with
        | Some p -> p
        | None -> CustomProperty iri

    // Optionally, for reverse-lookup: get all known IRIs for a property
    let iriOfProperty (prop:MorkPropertyX) : string =
        match prop with
        | CustomProperty iri -> iri
        | _ -> Prefixes.Mork + (prop :> NamedConcept).QName

*)

    // --- End of MorkProperty model ---