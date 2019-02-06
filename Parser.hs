-- This module parses Strings to Terms
module Parser 
    (
        parse
    ) 
where 

import Data.Maybe (fromJust)
import Data.Either
import Numeric.Natural
import Terms
import Data.List
import Data.List.Split (splitOn)

type Token = String 

data Operator = Plus
                | StarStar
                | Star
                | DivSlash
                | LnE
                | ExpFn
                | Minus
                | Lbr
                | Rbr
                deriving (Eq,Show)           

parse :: String -> Term
parse = termify . initialParse 

-- For testing purposes and to show general flow
initialParse :: String -> [Either Operator Term]
initialParse = detectVarsAndNumbers . (map tokenToOperator) . tokenize 

termify :: [Either Operator Term] -> Term
-- I've reached a core term, such as a Variable or a Number (Or an already calculated Term)
termify [Right t] = t
-- I've got something bigger
termify toks = 
    let 
        -- I get the first Operator (Method below)
        fst = firstOperator toks
        -- I split the List by the Lefthandside (LHS), Operator and Righthandside (RHS)
        (lhs,Left op,rhs) = splitByFirst toks fst
        -- I Pattermatch only for operators, so if i get a Term here it's a justified error
    in 
        -- I match the operators...
        case op of 
            -- If i got an opening bracket, I first go right from the bracket and termify that
            Lbr -> termify (lhs ++ [Right (termify rhs)])
            -- I got an closing bracket, i do everything i've got right first, and then return myself joined with rhs
            -- This only get everything that is RIGHT from the opening bracket, so i got no problems loosing anything
            -- After both together i have termify (lhs ++ termify (everything in Brackets) ++ rhs)
            Rbr -> termify (Right (termify lhs) : rhs)
            -- I've got an operator i've declared unary (unaries are below)
            _ | (elem op unaries) ->
                let 
                    -- Get the right Term and apply the Operator to form new Term
                    rnb = safeRight (head rhs)  -- right neighboor 
                    rhs' = tail rhs
                    step = applyUnary op rnb
                in 
                    -- Recursive step further with one Operator less
                    termify (lhs ++ (Right step) : rhs') 
            -- I've got an Operator i've declared binary (such as +)
              | (elem op binaries) -> 
                let
                    -- get Both Neighboors as Terms
                    rnb  = safeRight (head rhs) 
                    rhs' = tail rhs
                    lnb  = safeRight (last lhs) 
                    lhs' = init lhs
                    -- Apply the Operator to form a new Term
                    step = applyBinary lnb op rnb 
                in
                    -- Recursive Step further with one Operator less
                    termify (lhs' ++ [Right step] ++ rhs')
            otherwise       -> 
                Var "Err" --does this ever happen? Is this Smart?
    where 
        binaries = [Plus,Minus,Star,StarStar,DivSlash] 
        unaries  = [LnE,ExpFn]
        safeRight = fromRight (Var "Err") 
        applyUnary :: Operator -> Term -> Term 
        applyUnary op t = 
            case op of 
                LnE     -> Ln  t 
                ExpFn   -> Exp t
                -- Additional Unary Operators
                -- TODO: Negating
        applyBinary :: Term -> Operator -> Term -> Term 
        applyBinary a op b =
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
    where operators = [("(",Lbr),(")",Rbr),("Ln",LnE),("Exp",ExpFn),("**",StarStar),("*",Star),("/",DivSlash),("+",Plus),("-",Minus)]

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
precedence :: Either Operator Term -> (Natural, Either Operator Term)
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
                                        ExpFn -> 3
                                        Lbr -> 2
                                        Rbr -> 2
                    
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
lowestPrio :: [(Natural,Either Operator Term)] -> Natural
lowestPrio [] = 16
lowestPrio [(i,t)] = i
lowestPrio ((i,t):xs) = min i (lowestPrio xs)

--TODO: Why can't i load this from normal Data.Either Package?
fromRight :: b -> Either a b -> b 
fromRight def (Right b) = b 
fromRight def (Left a) = def