-- This File contains solving Terms
module Solve where

import Terms

solve :: Term -> [(String,Double)]-> Double
solve t vars = 
    case t of
        Add a b -> solve a vars + solve b vars
        Sub a b -> solve a vars - solve b vars
        Mul a b -> solve a vars * solve b vars
        Div a b  -> solve a vars / solve b vars
        Ln t -> log (solve t vars)
        Pow a b -> (solve a vars ** (solve b vars))
        Numb n -> n 
        Const c -> catchEmpty (resolveConstant c)
        Var v -> catchEmpty (resolveVariable v vars)