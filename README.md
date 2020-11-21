# statistics-probability

## Summary

A small project to calculate probabilities based on a given distribution.  For
Continuous Distributions we also have some calculus implemented.

## Prerequisites

+ [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
  (package manager)
+ Install HLint (linter)
  + [Via
    Stack](https://docs.haskellstack.org/en/stable/CONTRIBUTING/#code-quality)
  + [Via
    Cabal](https://github.com/ndmitchell/hlint#installing-and-running-hlint)

## Test

All tests are written under `test/`.

To run the tests, use the following command:
```bash
$ stack test
```

Or if you'd prefer to run them in the GHCi then:
```bash
$ stack ghci test/ExpressionsSpec.hs
ExpressionsSpec> main (or hspec <specific-spec>)
```

## Build and Use
All library source code is under `src/`.

You can boot them into the GHCi for use via:
```bash
$ stack ghci src/<FileName>.hs
```
