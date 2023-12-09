# BioHacker

BioHacker is a test bed for using AI and PL in biology. It started as a class project in Gerry Sussman's class at MIT and has grown into a research project on drug repurposing using Causality and Truth-Maintenance Systems. The project also contains an improved code base from the wonderful hacker book _[Building Problem Solvers](BPS)_.

## 2021: Teaching a (good old-fashioned) AI new tricks: probabilistic, causal and counterfactual reasoning with truth maintenance systems

See [the ProbProg 2021 poster (PDF)](https://probprog.cc/assets/posters/wed/28.pdf).

See [the code under BPS](https://github.com/namin/biohacker/tree/master/BPS#causality-toc).

## 2008: Debugging biological networks to reach coherence, completeness and consistency

See [the 2008 report (PDF)](http://lampwww.epfl.ch/~amin/doc/biohacker.pdf).

See [the code](https://github.com/namin/biohacker/tree/master/network-debugger).

_"Mistakes are the portals of discovery."_ -- James Joyce (1882-1941)

A metabolic network is defined as the set of biochemical reactions that the enzymes coded for in an organism's genome are capable of catalyzing. When the network of an organism is derived from pathway databases, it needs to be debugged for

* incoherences
  e.g., the metabolic model must obey mass balance laws.

* incompleteness
  e.g., the metabolic model is missing known reactions, genes, or compounds

* inconsistencies
  e.g., the metabolic model is inconsistent with experimental results

Because of these issues, reconstructing metabolic networks from annotated genomes is currently a time-consuming, error-prone process.

We develop BioHacker, a debugger for metabolic networks. BioHacker not only detects these issues, but also generates explanations as to where the problems lie and generates hypotheses for how to fix them.
