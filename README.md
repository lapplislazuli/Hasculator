# Hasculator 
Welcome to the Hasculator!

This project is done to get hands on Haskell, groove into the syntax and show some of it's utilities. 

The Hasculator can currently:

* parse a correct term with variables
* simplify a term (without using commutivity and associativity)
* solve a term given all variables
* differentiate a single-variable-term

But there will be more, of course!
## Overview
The top-class, Calculator.hs combines all parts and will (later) contain the main-method. 


Term.hs contains the primary data-structure Term, rules for simplification and solving.


Parser.hs contains primarily the function parse :: String -> Term and everything evolving around it. 
The precedence and error handling is also done here. 


Differentiator.hs contains a single method for differentiating a Term for a certain variable, but will contain more later 
(such as differentiating for every given variable and setting up a jakobi-matrix)


Solver.hs is currently empty but is meant to include solving f(x) for any given t, such as zero-points.
Additionally the solver should include finding Maxima and Minima.


Tests.hs contains the unit-tests of the project, grouped by different functions. Currently all tests are found inside this file.


Setup.hs is a supportive file for cabal, which will alter include dependencies and how to run tests.
## Feeling it
For a small demo, open the console and check these simple commands:

```
ghci> :load Calculator.hs 
ghci> let t = parse "2 + 3 * 1 + x"
ghci> let t' = simplify t
ghci> solve t' ["x",4]
ghci> diff t' "x"
```

To run the tests, you need to have HUnit installed. 

```
ghci> :load Tests.hs
ghci> runTestTT allTests
```

## Remarks
The core-code of differentation was transcripted from [Prof. Stroetmanns Setlx-Implementation](https://github.com/karlstroetmann/Logik/blob/master/SetlX/diff.stlx)

## Build with
* [HUnit](http://hackage.haskell.org/package/HUnit) v.1.6