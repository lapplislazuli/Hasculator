-- This module parses Strings to Terms
module Parser where 

import Data.Maybe (fromJust)
import Data.Either
import Terms
import Data.List
import Data.List.Split (splitOn)

type Token = String 

data Operator = Plus
                | StarStar
                | Star
                | DivSlash
                | LnE
                | Minus
                | Lbr
                | Rbr
                deriving (Eq,Show)           

-- For testing purposes and to show general flow
initialParse :: String -> [Either Operator Term]
initialParse s = detectVarsAndNumbers (map tokenToOperator (tokenize s))

termify :: [Either Operator Term] -> Term
termify [Right t] = t
termify toks = 
    let 
        fst = firstOperator toks
        (lhs,Left op,rhs) = splitByFirst toks fst
    in 
        case op of 
            Lbr -> termify (lhs ++ [Right (termify rhs)])
            Rbr -> termify (Right (termify lhs) : rhs)
            _ | (elem op unaries) ->
                let 
                    rnb = head rhs 
                    rhs' = tail rhs
                    step = termifyUnary op (fromRight' rnb)
                in 
                    termify (lhs ++ (Right step) : rhs') 
              | (elem op binaries) -> 
                let
                    rnb  = fromRight' (head rhs) 
                    rhs' = tail rhs
                    lnb  = fromRight' (last lhs) 
                    lhs' = init lhs
                    step = termifyBinary lnb op rnb 
                in
                    termify (lhs' ++ [Right step] ++ rhs')
            otherwise       -> 
                Var "Err" --does this ever happen? Is this Smart?
    where 
        binaries = [Plus,Minus,Star,StarStar,DivSlash] 
        unaries  = [LnE]    

termifyUnary :: Operator -> Term -> Term 
termifyUnary op t = 
    case op of 
        LnE     -> Ln t 
        -- Additional Unary Operators
        -- TODO: Negating

termifyBinary :: Term -> Operator -> Term -> Term 
termifyBinary a op b =
    case op of 
        StarStar    -> Pow a b
        Star        -> Mul a b 
        DivSlash    -> Div a b 
        Plus        -> Add a b
        Minus       -> Sub a b
        -- Additional Binary Operators 


tokenize :: String -> [Token]
tokenize s = words s 

tokenToOperator :: Token -> Either Operator Token
tokenToOperator t = 
    let mop = lookup t operators
    in 
        if mop == Nothing
        then Right t 
        else Left (fromJust mop)
    where operators = [("(",Lbr),(")",Rbr),("Ln",LnE),("**",StarStar),("*",Star),("/",DivSlash),("+",Plus),("-",Minus)]

detectVarsAndNumbers :: [Either Operator Token] -> [Either Operator Term]
detectVarsAndNumbers [] = []
detectVarsAndNumbers ((Left e):es) = (Left e) : detectVarsAndNumbers es 
detectVarsAndNumbers ((Right e):es) = (Right (tokenToTerm e)) : (detectVarsAndNumbers es)

tokenToTerm :: Token -> Term
tokenToTerm [] = Numb 0
tokenToTerm t@(s:ss) = if (not (elem s ['a'..'z'])) --s is a number, or atleast not a char
                        then Numb (read t)
                        else Var t --TODO: Catch Constants here

-- Assings the priority of each Term or Operator to the Term or operator
-- I left some out, i case i want to add more or i forgot something
--TODO: Do this with a List as Input 
precedence :: Either Operator Term -> (Int, Either Operator Term)
-- Finished Terms have the "lowest" Priority marked as 15
precedence (Right t) = (15, Right t)
-- Every Operator gets a priority
precedence (Left o) = (priorityOf o, Left o)
                        where priorityOf o =
                                    case o of 
                                        Plus -> 8
                                        Minus -> 8
                                        Star -> 6
                                        DivSlash -> 6
                                        StarStar -> 4
                                        LnE -> 3
                                        Lbr -> 2
                                        Rbr -> 2
                    --TODO: Lookup Unsigned Integers
                    
-- I Split by the Operator with the highest Priority and Return a Combination of the LefthandSide, Operator and RighthandSide
splitByFirst :: [Either Operator Term] -> Either Operator Term -> ([Either Operator Term],Either Operator Term,[Either Operator Term])
splitByFirst t ops = 
                let (p:ps) = splitOn [ops] t
                in (p, ops, intercalate [ops] ps)

firstOperator :: [Either Operator Term] -> Either Operator Term
firstOperator os =
                let l = map precedence os
                    lowest = lowestPrio l
                in  getFirstOperator lowest l
                where 
                    getFirstOperator low ((i,t):xs) =
                        if (i == low) 
                            then t
                            else getFirstOperator low (xs)

-- Find me the lowest priority in my current Operator/Term-List
lowestPrio :: [(Int,Either Operator Term)] -> Int
lowestPrio [] = 16
lowestPrio [(i,t)] = i
lowestPrio ((i,t):xs) = min i (lowestPrio xs)

--TODO: Why can't i load this from normal Data.Either Package?
fromRight :: b -> Either a b -> b 
fromRight def (Right b) = b 
fromRight def (Left a) = def

fromRight' = fromRight (Var "Err")