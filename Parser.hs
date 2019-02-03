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

termify:: [Either Operator Term] -> Term 
termify [] = undefined
-- I've reached a core-term, such as a variable or a Number
termify [Right t] = t
-- I've got Operators
termify t = let 
                fst = firstOperator t
                (lhs,Left op,rhs) = splitByFirst t fst 
                lnb' = last lhs 
                rnb'= head rhs 
                lleft = take (length lhs -1) lhs 
                rleft = drop 1 rhs 
            in 
                -- I check if i reached a term
                if (isRight fst)
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
                                LnE -> termify (lhs ++ Right (Ln rcasted) : rleft)
                                -- Binary Operator
                                StarStar -> termify (lleft ++ Right (Pow lcasted rcasted) : rleft)
                                Plus -> termify (lleft ++ Right (Add lcasted rcasted) : rleft)
                                Minus -> termify (lleft ++ Right (Sub lcasted rcasted) : rleft)
                                Star -> termify (lleft ++ Right (Mul lcasted rcasted) : rleft)
                                DivSlash -> termify (lleft ++ Right (Div lcasted rcasted) : rleft)
                                -- Any other stuff ?
                                otherwise -> undefined
                    else 
                        case op of 
                            Lbr -> termify (lhs ++ [Right (termify rhs)])
                            Rbr -> termify (Right (termify lhs) : rhs)
                            otherwise -> undefined 
                            -- TODO Something is wrong with "Ending with )" 

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