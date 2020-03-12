module Tests.TestHelpers  (
    module Tests.TestHelpers,
    module Test.HUnit,
    module Terms, 
    module Parser,
    module Solver,
    module Differentiator
)where 

import Test.HUnit
import Terms 
import Parser
import Solver
import Differentiator 

------------------------------------
-- Helpers
------------------------------------ 

simpSolDifPar s = solDifPar s []
solDifPar s = solve ((dx . parse) s) 

--These methods shorthand some tests
parseAndSolve :: String -> [(String, Double)] -> Double
parseAndSolve str = solve (parse str)
--Solves the Term for no-Variables (or every Variable 0)
simpleParseAndSolve :: String -> Double 
simpleParseAndSolve str = parseAndSolve str []

reparse :: Term -> Term 
reparse = parse . show

regulaFalsi' :: Term -> Double -> Double -> Double
regulaFalsi' t a b = removeErrDouble (regulaFalsi250 t a b)

removeErrDouble :: Either String Double -> Double
removeErrDouble (Right d) = d
removeErrDouble (Left e) = error e 

truncate' :: Double -> Int -> Double
truncate' x n = fromIntegral (floor (x * t)) / t
    where t = 10^n