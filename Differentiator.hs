-- This method uses calculus to differentiate 
module Differentiator where

import Terms

diff :: Term -> Char -> Term
diff t c=
    simplify (
        case t of 
            Add a b -> Add (diff a c ) (diff b c)
            Sub a b -> Sub (diff a c ) (diff b c )
            Mul (Numb n) t -> Mul (Numb n) (diff t c )
            Mul a b -> Add (Mul (diff a c) b) (Mul a (diff b c) )
            Div a b -> Div (Sub (Mul (diff a c ) b) (Mul a (diff b c )) ) (Mul b b)
            Ln t -> Div (diff t c) t
            Pow (Var v) (Numb n) -> 
                if c == v 
                then Mul (Numb n) (Pow (Var v) (Numb (n-1) ) )
                else undefined -- TODO: a^3 * x dx = a ^ 3
            Pow a b -> Mul (Pow (diff a c) b) (diff b c)
            Const c -> Numb 0
            Numb n -> Numb 0
            Var v -> diffVariable v c
    )

diffVariable :: Char -> Char -> Term
diffVariable v c =
        if (v == c) 
            then Numb 1 
            else Numb 0

--Shortcut for diffing x
dx :: Term -> Term
dx t = diff t 'x'