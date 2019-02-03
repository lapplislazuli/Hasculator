-- This module parses Strings to Terms
-- It contains tokens and Operators
module Parser where 

import Data.Maybe (fromJust)
import Terms
import Data.List

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

termify:: [Either Operator Term] -> Term 
-- No Operators left
termify [] = Numb 0 
-- I've reached a core-term, such as a variable or a number
termify [Right t] = t

-- I've got an prefix unary operator
termify ((Left fst):rest) 
    | fst == LnE = Ln (termify rest)
    | otherwise = undefined -- Here is also the Errorcase if i got (strangely) Operators left
-- I've got an Infix binary operator
termify ((Right fst):(Left snd):rest)
    | snd == StarStar = Pow fst (termify rest)
    | snd == DivSlash = Div fst (termify rest)
    | snd == Star = Mul fst (termify rest)
    | snd == Plus = Add fst (termify rest)
    | snd == Minus = Sub fst (termify rest)
    | otherwise = undefined -- Here are also error cases?