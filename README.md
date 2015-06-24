## A Static Library for Mandatory Access Control in Haskell

This is the repository for the hackage package MAC. When installed, MAC provides
Haskell with static guarantees on how data gets propagated within programs. MAC
enables untrusted code to manipulate sensitive data (i.e., secret information),
while preserving its confidentiality. MAC supports references, exceptions,
concurrency, and MVars.

This repository is the accompanying code for the paper
 [Functional
 Pearl: Two can keep a secret, if one of them uses Haskell](http://www.cse.chalmers.se/~russo/publications_files/pearl-russo.pdf).
