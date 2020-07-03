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
    - that are at different levels of resolution
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
    - that is find the closest cross links
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


# References to Dempster-Shafer Theory

Dempster-Shafer Theory
Glenn Shafer
http://www.glennshafer.com/assets/downloads/articles/article48.pdf

Dempster-Shafer Theory chapter (see 4.3 for Dempster's Combination Rule)
http://www.blutner.de/uncert/DSTh.pdf

Dempster-Shafer Theory slides
http://www.blutner.de/uncert/Dempster-Shafer.pdf

Combination of Evidence in Dempster-Shafer Theory, 2002
Kari Sentz, Scott Ferson
https://www.researchgate.net/publication/235419085_Combination_of_Evidence_in_Dempster-Shafer_Theory

Dempster's Rule of Combination
DEVELOPMENT OF A COMMON EDUCATIONAL AND TRAINING INFRASTRUCTURE
for the Integration of Remote Sensing, Digital Processing of Satellite Imagery,
Photointerpretation and GIS Methods, Techniques and Applications'
CO-ORDINATOR
NATIONAL TECHNICAL UNIVERSITY OF ATHENS,
LABORATORY OF REMOTE SENSING
Scientist in charge: Prof. D. ROKOS
http://portal.survey.ntua.gr/main/labs/rsens/DeCETI/IRIT/MSI-FUSION/node183.html

On the behavior of Dempster’s rule of combination, 2011
Jean Dezert, Albena Tchamova.
hal-00577983v1
https://hal.archives-ouvertes.fr/file/index/docid/577983/filename/OnBehaviorOfDSRule.pdf

A Simple View of the Dempster-Shafer Theory of Evidence and its Implication for the Rule of Combination, 1986
Lotfi A. Zadeh
http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.98.6349

The Unnormalized Dempster’s Rule of Combination: a New Justification from the Least Commitment Principleand some Extensions, May 7, 2010
Frederic Pichon, Thierry Denoeux
https://www.lgi2a.univ-artois.fr/~pichon/pdf/jar_final.pdf

Overview of Dempster-Shafer and Belief Function Tracking Method
Erik Blasch, Jean Dezert, B Pannetier
Advances and Applications of DSmT for Information Fusion. Collected Works. Volume 4
http://fs.unm.edu/OverviewDempsterShafer.pdf

An Introduction to Bayesian and Dempster-Shafer Data Fusion, 2005
Don Koks, Subhash Challa
http://robotics.caltech.edu/~jerma/research_papers/BayesChapmanKolmogorov.pdf

A Mathematical Theory of Evidence turns 40. International Journal of Approximate Reasoning 79 7-25. December 2016.
Glenn Shafer
http://www.glennshafer.com/assets/downloads/MathTheoryofEvidence-turns-40.pdf

http://www.glennshafer.com/books/amte.html

Perform Dempster's Rule of Combination
Code Golf
https://codegolf.stackexchange.com/questions/94719/perform-dempsters-rule-of-combination

Dempster's Rule As Seen By Little Colored Balls
https://dl.acm.org/doi/10.1111/j.1467-8640.2012.00421.x
