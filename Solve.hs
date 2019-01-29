-- This File contains solving Terms
module Solve where

import Terms

solve :: Term -> [(Char,Double)]-> Double
solve t vars = 
    case t of
        Addition a b -> solve a vars + solve b vars
        Substraction a b -> solve a vars - solve b vars
        Multiplication a b -> solve a vars * solve b vars
        Division a b  -> solve a vars / solve b vars
        Logarithm t -> log (solve t vars)
        Power a b -> (solve a vars ** (solve b vars))
        Value v -> (resolveLiteral v vars)