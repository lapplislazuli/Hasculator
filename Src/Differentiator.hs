-- This method uses calculus to differentiate 
module Differentiator where

import Terms

simpDiff :: Term -> String -> Term 
simpDiff t c = simplify (diff t c)

diff :: Term -> String -> Term
diff t c=
        case t of 
            Add a b -> Add (diff a c ) (diff b c)
            Sub a b -> Sub (diff a c ) (diff b c )
            Mul a b -> Add (Mul (diff a c) b) (Mul a (diff b c) )
            Div a b -> Div (Sub (Mul (diff a c ) b) (Mul a (diff b c )) ) (Mul b b)
            Ln t -> Div (diff t c) t
            Exp t -> Mul (Sub t (Numb 1)) (Exp t)
            Pow a b -> diff (Exp (Mul b (Ln a))) c
            Const c -> Numb 0
            Numb n -> Numb 0
            Var v -> diffVariable v c

diffVariable :: String -> String -> Term
diffVariable v c =
        if v == c
        then Numb 1 
        else Numb 0

--Shortcut for diffing x
dx :: Term -> Term
dx t = simpDiff t "x"