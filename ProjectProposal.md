# Introduction #
A metabolic network is defined as the set of biochemical reactions that the enzymes coded for in an organism's genome are capable of catalyzing.

For example,  a gene in a genome produces a protein product P1 that is capable of catalyzing a reaction Re1, which converts the small molecules A + B -> C.
Another gene produces a protein product P2 that is capable of catalyzing a reaction Re2, which converts small molecules C + D -> E.

In general, the full set of metabolic reactions constitutes a hypergraph network, where sets of reactants are converted to sets of products.

For any given sequenced organism, it is possible to infer the metabolic network given its genome.  However, oftentimes, incompleteness in the genome annotations will lead to "pathway holes" whereby essential metabolites necessary for growth cannot be produced.  Thus, it may be necessary to temporarily "assume" the existence of enzymes for which there is no evidence in order to generate hypotheses for further testing.

Furthermore, pathway databases from which the metabolic network was derived often contain stoichiometric inconsistencies that violate mass balance laws.  Thus, encoding mass balance as a constraint where violations can be detected would be quite valuable.

Due to these issues, reconstructing metabolic networks from annotated genomes is currently a time-consuming, error-prone process.
What is needed are tools to debug these metabolic networks as they are being constructed.  It would be helpful if not only were these errors detected, but also explanations generated as to where the problem lies, and possible suggestions as to how to fix it.


# Details #

We propose to develop a Truth Maintenance System to address these issues.  As a warm-up exercise, the TMS should be able to solve the following problem:

  1. Represent mass balance as a constraint into the TMS.  Given a set of metabolites some of which have missing masses, and a set of reactions, detect any stoichiometric inconsistencies.

Once we have detected and corrected all stoichiometric inconsistencies, we are left with a consistent set of reactions that comprises the metabolic network.  Now we wish to determine whether the network is complete.

> 2. Given a set of essential compounds, a set of reactions that can be catalyzed by the enzymes coded for in the genome, and assumptions about a particular nutrient media,  can these essential compounds be produced?

> 3. Given a Universal set of known biochemical reactions, which reactions should be added to the network to produce the unproduceable essential compounds?

If we include chemical formula-type information, then not only should it be able to do things like atomic balancing, but it should also be able to answer questions such as:

> 4. Given a 6-carbon compound such as glucose, is it possible to produce a 2-carbon compound such as acetate without losing any carbons? i.e.   C6H12O6  + O2 --> 3 C2H3O2 +   H2O + OH

If we include chemical structure information, and allow the representation of general patterns of compounds, then we would like it to be able to answer this kind of question:

> 5. A new, artificially sweetened, and heavily preserved food product from BrandX is currently waiting approval from the FDA. The FDA would like to know if the chemicals present in this product's ingredients will somehow combine with chemicals in the human body to produce harmful (carcinogenic, neurotoxic, dispeptic, etc) compounds. The FDA has a database of known toxins, but no way of knowing if a complex chain reaction stemming from the mixture of ingredient chemicals and human body chemicals might produce them.

If we represent the relationships of genes to the reactions that their products catalyze, then we have a complicated AND-OR network, where, if two proteins are members of an enzyme complex that catalyzes a reaction, then g1 AND g2 -> Re1.
On the other hand, if two proteins are both capable of independently catalyzing a reaction then, g3 OR g4 --> Re2.
Of course, any given reaction might be catalyzed by a group of protein complexes:  (OR (AND g5 g6) (AND g7 g8) ) --> Re3.
Using this formalism, it should be possible to ask the following sorts of questions:

> 6. For any given nutrient set, find the smallest set of genes that can still produce all the essential compounds necessary for growth.

Some engineering desiderata:

  * What pathway database to use?
    * BioCyc: written in lisp, developed by SRI.  Advantages: a very clean API to access data.  Contains chemical compounds with structures, a chemical ontology, reactions, genes, pathways.  Not all compounds have structures, some are generic compounds.  Some reactions are unbalanced.
    * KEGG:  A larger KB than Biocyc.  50% of compounds have no structure, many unbalanced reactions.
    * iAF1260:  A cleaner database, in that every reaction is balanced, and every metabolite has full structures associated with it

  * What type of TMS to use?
    * It seems that we can represent most of what we care about using simple horn clauses, so either a JTMS or an ATMS seems most appropriate
    * We may want to modify our JTMS so that instead of propagating in and out labels through the network, it records a derivation count with each justification that is the maximum number of justifications between it and an assumption or premise (infinity if the justification doesn't hold).  See Page 195, problem 8 of BPS.
  * It is currently unclear what kind of reasoner we want to use.
    * We will most likely roll our own, based on examples in class and the BPS book.
    * However, there are some open source  reasoners I (Jeremy) have previous experience with:
      * Snark: The advantage of Snark is that it is written in lisp, and has already been incorporated into BioBike as Biodeducta
      * Pellet: A java implementation of a Description Logic Reasoner.  Perhaps overkill for our needs.
      * FaCT:  A lisp implementation of a Description Logic Reasoner.
      * FaCT++:  A C++ implementation of FaCT, that is OWL 1.1 compliant.
  * What kind of query/constraint language do we want to develop?  Again, we will probably roll our own (structured in the SICP terms of primitives, means of combinations & means of abstractions), but there are several existing query languages that have been developed on top of lisp for this kind of domain:
    * Biovelo:  Developed by the Biocyc folks at SRI.  Already works out of the box with Biocyc
    * Biolingua:  Developed by Jeff Shrager for multiple databases. May need quite a bit of adapting
    * We will explore both of these languages, and adopt/adapt them as seems necessary.

The main goal of this task is to aid the metabolic reconstruction process, so emphasis will be placed on the clarity of the explanations provided by the TMS.

More ambitiously, we want to think of building a system that is capable of debugging a network on its own. Let's imagine that we can give the system the ability to represent the procedural knowledge necessary to debug a network. When given a network, if the system detects a bug, it classifies it in a taxonomy so that it can look up a strategy for patching the network. The strategy is implemented by vetting an instance against a set of critics that apply their aesthetics against the patch to see if it passes. If it does, the system applies the patch, stores it in a patch library and then re-evaluates the network looking for new bugs. If the patch doesn't pass, then a new patch is generated and the process repeats. When no more bugs are detected, the process halts.