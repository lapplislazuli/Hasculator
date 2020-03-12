# Hasculator

Welcome to the Hasculator!

This project is done to get hands on Haskell, groove into the syntax and show some of its utilities.

The Hasculator can currently:

* parse a correct term with variables
* simplify a term (without using commutativity and associativity)
* solve a term given all variables
* differentiate a single-variable-term

But there will be more, of course!

## Overview

The top-class, **Program.hs** combines all parts and will (later) contain the main-method.

**Term.hs** contains the primary data-structure *Term*, rules for simplification and solving.

**Parser.hs** contains primarily the function *parse :: String -> Term* and everything evolving around it.
The precedence and error handling is also done here.

**Differentiator.hs** contains a single method for differentiating a Term for a certain variable, but will contain more later
(such as differentiating for every given variable and setting up a Jakobi-matrix)

**Solver.hs** currently contains an implementation of the Regula Falsi to find zero points.

**Tests.hs** contains the unit-tests of the project, grouped by different functions. Currently all tests are found inside this file.

**Setup.hs** is a supportive file for cabal, which will alter include dependencies and how to run tests.

## Feeling it

For a small demo, open the console and check these simple commands:

```(shell)
ghci> :load Calculator.hs
ghci> let t = parse "2 + 3 * 1 + x"
ghci> let t' = simplify t
ghci> solve t' ["x",4]
ghci> regulaFalsi (parse "x - 2" 100 (-5) 5)
ghci> diff t' "x"
```

To run the tests, you need to have HUnit installed.

```(shell)
ghci> :load Tests.hs
ghci> runTestTT allTests
```

## Remarks

The core-code of differentiation was transcribed from [Prof. Stroetmanns SetLX-Implementation](https://github.com/karlstroetmann/Logik/blob/master/SetlX/diff.stlx)

## Build with

* [HUnit](http://hackage.haskell.org/package/HUnit) v.1.6
