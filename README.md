# equiv

This repository contains a very limited prototype implementation of the decision procedure described in this [report](https://www.dropbox.com/s/phkbrfendpa2e6w/cs4999_report1.pdf?dl=0) ([this](https://www.dropbox.com/s/uiov8ajz7suth7d/cs4999_report2.pdf?dl=0) elaborates more on the algorithm).

Here's a short summary: while we already have a decision procedure for the equivalence of NetKAT expressions, this project aims to decide whether a NetKAT term behaves like another NetKAT term when only certain behaviors are observed. That is, only require certain fields of packets to be preserved or be ignored altogether. Such specifications are given as regular expressions of _selectors_ that select certain fields of packets. Then, the regularity of NetKAT terms and specifications is exploited to decide whether they are equivalent.

To build, clone the repository and run `make` (OASIS should do most of the work). To run, do `./main.byte`. The main program runs a satisfaction check on three sample specifications and associated NetKAT programs. Here's a breakdown of the modules:

Taken from the `netkat-automata` (the coalgebraic decision procedure) repository:
* `Frenetic_Decide_Ast`
* `Frenetic_Decide_Deriv`
* `Frenetic_Decide_Util`

Adapted from Steffen's (@smolkaj) ProbNetKAT modules for marshalling packets as strictly positive integers:
* `Frenetic_Decide_Packet_Repr` -- I've modified it to work with `Frenetic_Decide_Util` so it is no longer compatible with Steffen's modules. Perhaps we should create a common base project for these utilities...

Here are my original contributions:
* `Frenetic_Decide_Predicate` -- structures representing NetKAT predicates
* `Frenetic_Decide_Spec` -- represents specifications as regular expressions
* `Frenetic_Decide_SpecDeriv` -- Brzozowski derivatives of specifications
* `Frenetic_Decide_FA` -- a mini library for finite automata construction and manipulation. This is used to get a uniform representation of NetKAT term/specification automata from derivatives for labelling and expansion
* `Frenetic_Decide_Enum` -- a wrapper around `Frenetic_Decide_Packet_Repr` to lazily generate entire alphabets of packet for NetKAT automata
* `main` -- the main program. Contains the following functions:
  - `simulates`, which computes whether one DFA simulates another using the standard worklist/relation algorithm. However, it brute forces the computation of their alphabets, which we discuss below.
  - `check_satisfaction`, which computes whether one NetKAT term is equivalent to another up-to a specification. The algorithm is described [here](https://www.dropbox.com/s/s8eq5fdk6jgy69v/automata-relabeling-satisfaction.pdf?dl=0).

Nomenclature: a false positive is when the program says a NetKAT term `t2` is equivalent to `t1` up-to specification `s` when in fact they aren't, and vice versa for false negatives.

## TODO

There's a lot to be done! Currently, satisfaction up-to arbitrary predicates has been disabled since we do not know how to translate those semantics into functions over sets of packets.

Unfortunately, the labelling scheme and simulation check is incredibly naive and performs a brute-force calculation of the finite (but prohibitively large for nontrivial programs) alphabets of NetKAT automata. As a result, running this procedure on anything more than a trivial example will crash it. Steffen and I have been discussing two possible ways to symbolically represent sets of packets:

* Co-opting _bases_ from the coalgebraic decision procedure to reduce the impact of enumerating packets with shared values
* Nominal automata, to consider infinite packet alphabets altogether

Furthermore, the explicit conversion between derivatives and finite automata is expensive, so perhaps it would be better if we reformulated this procedure to act on derivatives directly.

Also, please let me know about any bugs (false positives/negatives) as well as nontermination, as that has been a problem.

