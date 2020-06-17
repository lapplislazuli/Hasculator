-- This Module defines my own Datatype Term
-- and its most important methods for solving, handling variables and simplifications

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
            | Sin Term
            | Cos Term
            | Const String
            | Var String
            | ErrorTerm String
            deriving (Eq)

-- | This method solves a given Term for a given set of variables. 
-- For any occuring variable where no value is passed, it treats the variable as 0 
-- For any erronous Term (such as, if a parsing error occurred), it returns 0
-- This behavior of error-terms and not-defined variables is applied at the bottom most level first, it tries to "heal" as much as possible.
-- Examples of healing: 
-- @
-- t <- parse "a + (1 -"
-- solve t [(a,5)] 
-- 5
-- @
-- 
-- For all mathematical operators and methods the base-library pardons have been used. 
solve :: Term               -- ^ The Term to solve 
    -> [(String,Double)]    -- ^ A list of its variables and their values 
    -> Double               -- ^ The result of the Term, if all variables are 
solve t vars = 
    case t of
        Add a b -> solve a vars + solve b vars
        Sub a b -> solve a vars - solve b vars
        Mul a b -> solve a vars * solve b vars
        Div a b  -> solve a vars / solve b vars
        Ln t -> log (solve t vars)
        Pow a b -> solve a vars ** solve b vars
        Exp t -> solve (Pow (Const "e") t) vars
        Numb n -> n 
        Const c -> catchEmpty (resolveConstant c)
        Var v -> catchEmpty (resolveVariable v vars)
        Sin u -> sin (solve u vars)
        Cos u -> cos (solve u vars)
        ErrorTerm _ -> 0

-- | This method tries to simplify a Term using algebraic identity laws. 
-- It is quite verbatim, but most cases are trivial. 
-- 
-- The first checked case is whether the Term has no variables - than it can simply be solved and returned. 
-- In case the term has variables, it tries to apply simple rules with pattern matches, 
-- such as "a + 0 = a" or "b * 1 = b"
-- In case no such pattern is found, the simplification is applied to both branches of the trees, possibly simplifying them.
-- A list of identities is found on Wikipedia (https://en.wikipedia.org/wiki/Identity_(mathematics)), but not all of those are useful for simplification.
--
-- This method should be applied multiple times as it performs only local optimization, and should be repeated until the Term does not change anymore. 
simplify :: Term -> Term
simplify t = 
    if not (hasVariables t)
    then Numb (solve t [])
    else 
        case t of
            -- Basics, to catch trivial end-cases
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
            (Pow (Numb 1) _)    -> Numb 1 
            (Ln (Numb 1))       -> Numb 0
            (Div a (Numb 1))    -> a 
            (Div (Numb 0) _ )   -> Numb 0 
            -- Shortcuts including one layer lower evaluation
            (Sub a b)   -> let  a' = simplify a 
                                b' = simplify b 
                            in  if a' == b' 
                                then Numb 0 
                                else Sub a' b'
            (Div a b)   -> let  a' = simplify a 
                                b' = simplify b 
                            in  if a' == b' 
                                then Numb 1 
                                else Div a' b'
            -- No further shortcuts - simply simplify the arguments 
            (Mul a b)   -> Mul (simplify a) (simplify b)
            (Pow a b)   -> Pow (simplify a) (simplify b)
            (Add a b)   ->  Add (simplify a) (simplify b)      
            (Ln u)      -> Ln (simplify u)
            (Exp u)     -> Exp (simplify u)
            (Sin u)     -> Sin (simplify u)
            (Cos u)     -> Cos (simplify u)
            _   -> t -- This wildcard helps to add further operators without rewriting code or running into a pattern-match error - it should be kept. 

-- | This method runs over a term and finds every occurence of a variable.
-- It does so in a left-to-right fashion. 
-- Variables which occur twice in the Term occur twice in the list.
-- 
-- It does not return a set, as for example the global occurence of a variable could disturb local simplification.
-- Also, building a set from a string is easy if required. 
extractVariables :: Term -> [String]
extractVariables (Var c) = [c]
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
hasVariables t =  not $ null (extractVariables t)
            
resolveConstant :: String -> Maybe Double
resolveConstant c = resolveVariable c constants

constants :: [(String,Double)]
constants = [(['e'], exp 1)]

resolveVariable :: String -> [(String,Double)] -> Maybe Double
resolveVariable _ [] = Nothing
resolveVariable i ((c,v):vs) = 
    if i == c 
    then Just v 
    else resolveVariable i vs
    
-- | This method resolves unknown and empty Variables to a value of 0
catchEmpty :: Maybe Double -> Double
catchEmpty Nothing = 0
catchEmpty (Just a) = a

negateTerm :: Term -> Term 
negateTerm = Mul (Numb (-1))

instance Show Term where 
    show t = 
        case t of 
             Numb a     ->  show a 
             Var a      ->  a 
             ErrorTerm err ->  err
             Const a ->  a 
             Add a b -> binaryShowHelper "+" a b 
             Sub a b -> binaryShowHelper "-" a b 
             Mul a b -> binaryShowHelper "*" a b 
             Div a b -> binaryShowHelper "/" a b 
             Pow a b -> binaryShowHelper "^" a b 
             Ln t -> unaryShowHelper "Ln" t 
             Exp t -> unaryShowHelper "Exp" t
             Sin t -> unaryShowHelper "sin" t
             Cos t -> unaryShowHelper "cos" t
        where 
            binaryShowHelper :: String -> Term -> Term -> String 
            binaryShowHelper op a b = "("++ show a ++ op ++ show b ++ ")"
            unaryShowHelper :: String -> Term -> String 
            unaryShowHelper op t = op ++ "(" ++ show t ++ ")"