-- This method uses calculus to differentiate 
module Differentiator where

import Terms

diff :: Term -> Term
diff t =
    simplify (
        case t of 
            Addition a b -> Addition (diff a) (diff b)
            Substraction a b -> Substraction (diff a) (diff b)
            Multiplication (Value (Number n)) t -> Multiplication (Value (Number n)) (diff t)
            Multiplication a b -> Addition (Multiplication (diff a) b) (Multiplication a (diff b) )
            Division a b -> Division (Substraction (Multiplication (diff a) b) (Multiplication a (diff b) )) (Multiplication b b)
            Logarithm t -> Division (diff t) t
            Power (Value (Variable c)) (Value (Number n)) -> Multiplication (Value (Number n)) (Power (Value (Variable c)) (Value (Number (n-1) )) )
            Power a b -> Multiplication (Power (diff a) b) (diff b)
            Value l -> Value (diffLiteral l)
    )

diffLiteral :: Literal -> Literal
diffLiteral l = 
    case l of 
        Number l -> Number 0
        Constant c -> Number 0
        Variable v -> Number 1