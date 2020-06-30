# TODOs

- translate
  - BPS/ltms/ltms.lisp to BPS/ltms/ltms.rkt
  - BPS/ltms/ltms-ex.lisp to BPS/ltms/ltms-ex.rkt

- (DONE) translate
  - BPS/ltms/jtms.lisp to BPS/ltms/jtms.rkt
  - BPS/ltms/jtms-ex.lisp to BPS/ltms/jtms-ex.rkt

- Done(namin): show how to run the racket code.
  - [BPS/RACKET.md](BPS/RACKET.md)

# JTMS in mediKanren

- high level inference rules to reduce hopping
  - gene 1 uprelates another gene 2
    - then can infer that any drug which uprelates gene 1 also uprelates gene 2, and maybe does more...
  - different ontologies
    - traverse two ontologies simultaenously
    - that are at different leevls of resolution
      - needs to understand the relationship between the two
      - e.g. create a mapping between the two
        - map between concepts or predicates
      - two ontologies having to do with diseases
      - in one ontology, 1 type of diabetes vs 10 types of diabetes
      - clustering? these drugs are in the same class
      - can we use a galois connection?
  - need the schemas of the ontologies
    - predicates
    - concepts
    - mapping between ontologies

- problem statement 1 (real!)
  - given two ontologies
  - given the synonyms
  - automatically infer a mapping between the ontologies
  - perhaps propose multiple mappings and need to choose among them
  - can use concrete/abstract data to do so
  - simple question:
    - here is a concept from one ontology
    - what is the closest concept in another ontology?

- problem statement 2 (problem statement 1 is an instance!)
  - we have pages of racket code intermingly query and logic
  - we have some high level problems that are too high level for a query graph
  - could you build an expert system in mediKanren?
  - can we encode higher level inference rules that serve as the logic glue where we now use ad-hoc racket code

- problem statement 3
  - abductive reasoning (need LTMS, probably)
  - evidence on a patient (genes, medication, symptoms, past diagnoses)
  - find explanations for their symptoms
  - require a probabilistic graphical model
    - the model encodes all medical knowledge (challenge)
    - with conditional probabilities
    - we fill in the things that we know
    - by bayesian reasoning, figure out the most likely
      - causes of symptoms
      - treatments of symptoms
  - to address challenge?
    - how do we get the probabilities?
      - we will have new clinical data with probabilities
  - pull neigborhoods around the focus to allow reasoning

# mediKanren

- tms + tre using mediKanren as a database to look facts in?
- what higher-level reasoning would it enable?
- augment jtms with beliefs: https://arxiv.org/pdf/1304.3084.pdf
- augment ltms with beliefs: https://aaaipress.org/Papers/Symposia/Fall/1993/FS-93-01/FS93-01-019.pdf
- go ontology explorer: https://www.ebi.ac.uk/QuickGO/

# Random Ideas

- pattern-directed chess
- reflection that uses a tms to guide its reasoning
