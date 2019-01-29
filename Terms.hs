-- This Module defines my own Datatype Term

module Terms where

data Term = Addition Term Term
            | Substraction Term Term
            | Multiplication Term Term
            | Division Term Term
            | Logarithm Term
            | Power Term Term
            | Value Literal

data Literal =  Number Int
                | Variable Char
                | Constant Char


constants :: [(Char,Double)]
constants = ('e', (exp 1)):[]

resolveConstant :: Char -> Maybe Double
resolveConstant c = resolveVariable c constants

resolveVariable :: Char -> [(Char,Double)] -> Maybe Double
resolveVariable _ [] = Nothing
resolveVariable i ((c,v):vs) = 
    if i == c 
    then Just v 
    else resolveVariable i vs


resolveLiteral :: Literal ->[(Char,Double)] -> Double
resolveLiteral l cs = 
    case l of 
        Number n -> (fromIntegral n)
        Constant c -> catchEmpty (resolveConstant c)
        Variable v -> catchEmpty (resolveVariable v cs)
    
catchEmpty :: Maybe Double -> Double
catchEmpty Nothing = 0
catchEmpty (Just a) = a 