-- This module parses Strings to Terms
-- It contains tokens and Operators
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

tokenToOperator :: Token -> Either Operator Token
tokenToOperator t = 
    let mop = lookup t operators
    in 
        if mop == Nothing
        then Right t 
        else Left (fromJust mop)
    where operators = [("(",Lbr),(")",Rbr),("Ln",LnE),("**",StarStar),("*",Star),("/",DivSlash),("+",Plus),("-",Minus)]

tokenize :: String -> [Token]
tokenize s = words s 

-- Assings the priority of each Term or Operator to the Term or operator
-- I left some out, i case i want to add more or i forgot something
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


detectVarsAndNumbers :: [Either Operator Token] -> [Either Operator Term]
detectVarsAndNumbers [] = []
detectVarsAndNumbers ((Left e):es) = (Left e) : detectVarsAndNumbers es 
detectVarsAndNumbers ((Right e):es) = (Right (tokenToTerm e)) : (detectVarsAndNumbers es)

tokenToTerm :: Token -> Term
tokenToTerm [] = Numb 0
tokenToTerm t@(s:ss) = if (not (elem s ['a'..'z'])) --s is a number, or atleast not a char
                        then Numb (read t)
                        else Var t --TODO: Catch Constants here

-- For testing purposes and to show general flow
initialParse :: String -> [(Int,Either Operator Term)]
initialParse s = map precedence (detectVarsAndNumbers (map tokenToOperator (tokenize s)))

termify:: [(Int,Either Operator Term)] -> Term 
termify [] = undefined
-- I've reached a core-term, such as a variable or a Number
termify [(_,Right t)] = t
-- I've got Operators
termify t = let 
                fst@(p,fstO) = firstOperator t
                (lhs,Left op,rhs) = splitByFirst t fst 
                lnb@(_,lnb') = last lhs 
                rnb@(_,rnb')= head rhs 
                lleft = take (length lhs -1) lhs 
                rleft = drop 1 rhs 
            in 
                -- I check if i reached a term
                if (isRight fstO)
                then termify [fst]
                else
                    -- I check if my neigboors are Terms?
                    if((isRight lnb') && (isRight rnb'))
                    then 
                        let 
                            lcasted = fromRight (Numb 0) lnb'
                            rcasted = fromRight (Numb 0) rnb'
                        in
                            case op of 
                                -- Unary Operator
                                LnE -> termify (lhs ++ (15,Right (Ln rcasted)) : rleft)
                                -- Binary Operator
                                Plus -> termify (lleft ++ (15,Right (Add lcasted rcasted)) : rleft)
                                Minus -> termify (lleft ++ (15,Right (Sub lcasted rcasted)) : rleft)
                                Star -> termify (lleft ++ (15,Right (Mul lcasted rcasted)) : rleft)
                                DivSlash -> termify (lleft ++ (15,Right (Div lcasted rcasted)) : rleft)
                                -- Any other stuff ?
                                otherwise -> undefined
                    else undefined

-- I Split by the Operator with the highest Priority and Return a Combination of the LefthandSide, Operator and RighthandSide
splitByFirst :: [(Int,Either Operator Term)] -> (Int,Either Operator Term) -> ([(Int,Either Operator Term)],Either Operator Term,[(Int,Either Operator Term)])
splitByFirst [] ops@(_,fst) = undefined
splitByFirst t@(x:xs) ops@(_,fst) = 
                        let (p:ps) = splitOn [ops] t
                        in (p, fst, localConc ps )
                        where localConc (d:ds) = d++ (localConc ds)
                              localConc [] = []


firstOperator :: [(Int,Either Operator Term)] -> (Int,Either Operator Term)
firstOperator l@((i,t):xs) = 
                            if i == lowest 
                                then (i,t)
                                else firstOperator(xs)
                            where lowest = lowestPrio l -- TODO: Move this somewhere else, it only needs to be called once

-- Find me the lowest priority in my current Operator/Term-List
lowestPrio :: [(Int,Either Operator Term)] -> Int
lowestPrio [] = 16
lowestPrio [(i,t)] = i
lowestPrio ((i,t):xs) = min i (lowestPrio xs)

--TODO: Why can't i load this from normal Data.Either Package?
fromRight :: b -> Either a b -> b 
fromRight def (Right b) = b 
fromRight def (Left a) = def