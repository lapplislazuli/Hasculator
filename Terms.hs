-- This Module defines my own Datatype Term

module Terms 
    (
        Term(..),
        solve,
        negateTerm,
        simplify, 
        extractVariables,
        hasVariables,
        constants
    )
where

data Term = Add Term Term
            | Sub Term Term
            | Mul Term Term
            | Div Term Term
            | Ln Term
            | Exp Term
            | Pow Term Term
            | Numb Double
            | Const String
            | Var String
            | ErrorTerm String
            deriving (Eq)

solve :: Term -> [(String,Double)]-> Double
solve t vars = 
    case t of
        Add a b -> solve a vars + solve b vars
        Sub a b -> solve a vars - solve b vars
        Mul a b -> solve a vars * solve b vars
        Div a b  -> solve a vars / solve b vars
        Ln t -> log (solve t vars)
        Pow a b -> (solve a vars ** (solve b vars))
        Exp t -> solve (Pow (Const "e") t) vars
        Numb n -> n 
        Const c -> catchEmpty (resolveConstant c)
        Var v -> catchEmpty (resolveVariable v vars)
        ErrorTerm _ -> 0

simplify :: Term -> Term
simplify t = 
    if (not (hasVariables t))
    then Numb (solve t [])
    else 
        case t of
            -- Basics 
            (Var c) -> Var c 
            (Const c) -> Const c 
            (Numb i) -> Numb i
            -- Simple Shortcuts
            (Add t (Numb 0))    -> t
            (Add (Numb 0) t)    -> t
            (Mul t (Numb 1))    -> t
            (Mul (Numb 1) t)    -> t 
            (Mul t (Numb 0))    -> Numb 0
            (Mul (Numb 0) t)    -> Numb 0
            (Pow _ (Numb 0))    -> Numb 1
            (Pow t (Numb 1))    ->  t
            (Ln (Numb 1))       -> Numb 0
            (Div a (Numb 1))    -> a 
            (Div (Numb 0) _ )   -> Numb 0 
            -- No Shortcuts found - go one layer deeper
            (Add a b)   -> Add (simplify a) (simplify b)
            (Sub a b)   -> Sub (simplify a) (simplify b)
            (Mul a b)   -> Mul (simplify a) (simplify b)
            (Div a b)   -> Div (simplify a) (simplify b)
            (Pow a b)   -> Pow (simplify a) (simplify b)
            (Ln t)      -> Ln (simplify t)
            (Exp t)     -> Exp (simplify t)

extractVariables :: Term -> [String]
extractVariables (Var c) = c:[]
extractVariables (Numb _) = [] -- Any Value that is not a variable will yield []
extractVariables (Const _) = [] -- Any Value that is not a variable will yield []
extractVariables (Add a b) = extractVariables a ++ extractVariables b
extractVariables (Sub a b) = extractVariables a ++ extractVariables b
extractVariables (Mul a b) = extractVariables a ++ extractVariables b
extractVariables (Div a b) = extractVariables a ++ extractVariables b
extractVariables (Pow a b) = extractVariables a ++ extractVariables b
extractVariables (Ln a)  = extractVariables a
extractVariables (Exp a) = extractVariables a

hasVariables :: Term -> Bool
hasVariables t =  length (extractVariables t) > 0
            
resolveConstant :: String -> Maybe Double
resolveConstant c = resolveVariable c constants

constants :: [(String,Double)]
constants = (['e'], (exp 1)):[]

resolveVariable :: String -> [(String,Double)] -> Maybe Double
resolveVariable _ [] = Nothing
resolveVariable i ((c,v):vs) = 
    if i == c 
    then Just v 
    else resolveVariable i vs
    
catchEmpty :: Maybe Double -> Double
catchEmpty Nothing = 0
catchEmpty (Just a) = a

negateTerm :: Term -> Term 
negateTerm t = Mul (Numb (-1)) t

instance Show Term where 
    show t = 
        case t of 
             Numb a     ->  show a 
             Var a      -> show a 
             ErrorTerm err -> show err
             Const a -> show a 
             Add a b -> binaryShowHelper "+" a b 
             Sub a b -> binaryShowHelper "-" a b 
             Mul a b -> binaryShowHelper "*" a b 
             Div a b -> binaryShowHelper "/" a b 
             Pow a b -> binaryShowHelper "^" a b 
             Ln t -> unaryShowHelper "Ln" t 
             Exp t -> unaryShowHelper "Exp" t
        where 
            binaryShowHelper :: String -> Term -> Term -> String 
            binaryShowHelper op a b = "("++ show a ++ op ++ show b ++ ")"
            unaryShowHelper :: String -> Term -> String 
            unaryShowHelper op t = op ++ "(" ++ show t ++ ")"