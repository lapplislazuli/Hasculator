-- This Module defines my own Datatype Term

module Terms where

data Term = Add Term Term
            | Sub Term Term
            | Mul Term Term
            | Div Term Term
            | Ln Term
            | Pow Term Term
            | Numb Double
            | Const Char
            | Var Char
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
    
catchEmpty :: Maybe Double -> Double
catchEmpty Nothing = 0
catchEmpty (Just a) = a

extractVariables :: Term -> [Char]
extractVariables (Var c) = c:[]
extractVariables (Numb _) = [] -- Any Value that is not a variable will yield []
extractVariables (Const _) = [] -- Any Value that is not a variable will yield []
extractVariables (Add a b) = extractVariables a ++ extractVariables b
extractVariables (Sub a b) = extractVariables a ++ extractVariables b
extractVariables (Mul a b) = extractVariables a ++ extractVariables b
extractVariables (Div a b) = extractVariables a ++ extractVariables b
extractVariables (Pow a b) = extractVariables a ++ extractVariables b
extractVariables (Ln a)  = extractVariables a

simplify :: Term -> Term
--Things i can really simplify
simplify (Add t (Numb 0)) = simplify t
simplify (Mul t (Numb 1)) = simplify t 
simplify (Mul t (Numb 0)) = Numb 0
simplify (Pow _ (Numb 0)) = Numb 1
simplify (Pow t (Numb 1)) = simplify t
simplify (Ln (Numb 1)) = Numb 0
simplify (Div a (Numb 1)) = a 
simplify (Div (Numb 0) _ ) = Numb 0
-- Simplify one layer deeper
simplify (Add a b) = Add (simplify a) (simplify b)
simplify (Sub a b) = Sub (simplify a) (simplify b)
simplify (Mul a b) = Mul (simplify a) (simplify b)
simplify (Div a b) = Div (simplify a) (simplify b)
-- If i got nothing else 
simplify t = t