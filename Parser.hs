-- This module parses Strings to Terms
-- It contains tokens and Operators
module Parser where 

import Data.Maybe (fromJust)
import Terms 

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
initialParse :: String -> [Either Operator Term]
initialParse s = detectVarsAndNumbers (map tokenToOperator (tokenize s))