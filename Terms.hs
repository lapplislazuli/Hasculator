-- This Module defines my own Datatype Term

module Terms where

data Term = Addition Term Term
            | Substraction Term Term
            | Multiplication Term Term
            | Division Term Term
            | Logarithm Term
            | Power Term Term
            | Value Literal
            deriving (Eq,Show)

data Literal =  Number Int
                | Variable Char
                | Constant Char
                deriving (Eq,Show)

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

extractVariables :: Term -> [Char]
extractVariables (Value (Variable c)) = c:[]
extractVariables (Value (Number _)) = [] -- Any Value that is not a variable will yield []
extractVariables (Value (Constant _)) = [] -- Any Value that is not a variable will yield []
extractVariables (Addition a b) = extractVariables a ++ extractVariables b
extractVariables (Substraction a b) = extractVariables a ++ extractVariables b
extractVariables (Multiplication a b) = extractVariables a ++ extractVariables b
extractVariables (Division a b) = extractVariables a ++ extractVariables b
extractVariables (Power a b) = extractVariables a ++ extractVariables b
extractVariables (Logarithm a)  = extractVariables a

simplify :: Term -> Term
--Things i can really simplify
simplify (Addition t (Value (Number 0))) = simplify t
simplify (Multiplication t (Value (Number 1))) = simplify t 
simplify (Multiplication t (Value (Number 0))) = Value (Number 0)
simplify (Power _ (Value (Number 0))) = Value (Number 1)
simplify (Power t (Value (Number 1))) = simplify t
simplify (Logarithm (Value (Number 1))) = Value (Number 0)
simplify (Division a (Value (Number 1))) = a 
simplify (Division (Value (Number 0)) _ ) = Value (Number 0) 
-- Simplify one layer deeper
simplify (Addition a b) = Addition (simplify a) (simplify b)
simplify (Substraction a b) = Substraction (simplify a) (simplify b)
simplify (Multiplication a b) = Multiplication (simplify a) (simplify b)
simplify (Division a b) = Division (simplify a) (simplify b)
-- If i got nothing else 
simplify t = t