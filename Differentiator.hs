-- This method uses calculus to differentiate 
module Differentiator where

import Terms

diff :: Term -> Char -> Term
diff t c=
    simplify (
        case t of 
            Addition a b -> Addition (diff a c ) (diff b c)
            Substraction a b -> Substraction (diff a c ) (diff b c )
            Multiplication (Value (Number n)) t -> Multiplication (Value (Number n)) (diff t c )
            Multiplication a b -> Addition (Multiplication (diff a c) b) (Multiplication a (diff b c) )
            Division a b -> Division (Substraction (Multiplication (diff a c ) b) (Multiplication a (diff b c )) ) (Multiplication b b)
            Logarithm t -> Division (diff t c) t
            Power (Value (Variable v)) (Value (Number n)) -> 
                if c == v 
                then Multiplication (Value (Number n)) (Power (Value (Variable v)) (Value (Number (n-1) )) )
                else undefined -- TODO: a^3 * x dx = a ^ 3
            Power a b -> Multiplication (Power (diff a c) b) (diff b c)
            Value l -> Value (diffLiteral l c)
    )

diffLiteral :: Literal -> Char -> Literal
diffLiteral (Variable v) c = 
    if v==c 
        then Number 1 
        else Number 0
diffLiteral _ _ = Number 0