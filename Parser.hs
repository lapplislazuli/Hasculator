-- This module parses Strings to Terms
module Parser 
where 

import Data.Maybe (fromJust,isJust)
import Data.Either
import Numeric.Natural
import Terms
import Data.List
import Data.List.Split (splitOn)


parse :: String -> Term
parse = termify . detectVarsAndNumbers . (map tokenToOperator) . tokenize'  

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

dictionary :: [(Token,Operator,Natural)]
dictionary = [("(",Lbr,2),(")",Rbr,2),("Ln",LnE,3),("Exp",ExpFn,3),("^",StarStar,3),("*",Star,6),("/",DivSlash,6),("+",Plus,9),("-",Minus,9)]

termify :: [Either Operator Term] -> Term
termify [] = Var "EmptyTermErr"
termify [Left op] = Var "LostOperatorErr"
-- I've reached a single core term, such as a Variable or a Number (Or an already calculated Term)
termify [Right t] = t
-- I've got something bigger
termify toks
    | (isRight next)= Var "MissingOperatorErr" -- I've got a Term in a List of Tokens as next - this should never be possible! Happens if i put (1 + 2 2 3)
    | (isLeft next) =
        let 
            op = safeLeft next 
        in 
            -- I match the operators...
            case op of 
                -- If i got an opening bracket, I first go right from the bracket and termify that
                Lbr -> termify (lhs ++ (applyBracket rhs))
                -- Rbr -> termify ((applyBracket op lhs) ++ rhs) --This should not ever be the case?
                Rbr -> Var "LostClosingBracketErr"
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
                _ | (elem op binaries) -> 
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
    where 
       binaries = [Plus,Minus,Star,StarStar,DivSlash] 
       unaries  = [LnE,ExpFn]
       -- I split the List by the Lefthandside (LHS), Operator and Righthandside (RHS)
       (lhs,next,rhs) = splitByFirst toks

applyBracket :: [Either Operator Term] -> [Either Operator Term]
applyBracket toks
    | isLeft o =
        let 
            op = safeLeft o
        in
            case op of 
                Lbr         -> applyBracket (l ++ (applyBracket r))
                Rbr         -> (Right (termify l)) : r
                otherwise   -> toks -- I've got every Bracket processed!
    | isRight o = toks
    where (l,o,r) = splitByFirst toks

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

tokenToOperator :: Token -> Either Operator Token
tokenToOperator t = 
    let mop = lookup t ( map (\(a,b,c) -> (a,b)) dictionary)
    in 
        if mop == Nothing
        then Right t 
        else Left (fromJust mop)

detectVarsAndNumbers :: [Either Operator Token] -> [Either Operator Term]
detectVarsAndNumbers lst = map f lst 
            where f (Right e) = Right (tokenToTerm e )
                  f (Left e) = Left e   

tokenToTerm :: Token -> Term 
tokenToTerm tok@(s:ss) 
            | not (elem s ['a'..'z'])       = Numb (read tok :: Double)
            | elem tok (map fst constants)  = Const tok
            | otherwise                     = Var tok

-- Assings the priority of each Term or Operator to it
precedence :: Either Operator Term -> (Natural, Either Operator Term)
precedence o = (priorityOf o,o)
    where 
        priorityOf (Right x) =  15
        priorityOf (Left x) = fromJust (lookup x ( map (\(a,b,c) -> (b,c)) dictionary))

splitByFirst :: [Either Operator Term] -> ([Either Operator Term],Either Operator Term,[Either Operator Term])
splitByFirst toks = splitBy (firstOperator toks) toks

splitBy :: Either Operator Term -> [Either Operator Term] -> ([Either Operator Term],Either Operator Term,[Either Operator Term])
splitBy ops toks= 
                let (p:ps) = splitOn [ops] toks
                in (p, ops, intercalate [ops] ps)

firstOperator :: [Either Operator Term] -> Either Operator Term
firstOperator os = snd (foldl step (17,Right (Var "PriorityErr")) (map precedence os))
                        where 
                            step old@(a,b) new@(c,d) 
                                | a > c= new
                                | otherwise = old

tokenize' :: String -> [String]
tokenize' s = filter (\n -> not (n==[])) (applySeps seps (words s))
    where 
        seps = (map (\(a,b,c) -> a :: String ) dictionary)
        applySep sep s' = flat' (map (split' sep) s')
        applySeps [] s' = s'
        applySeps (x:xs) s' = applySeps xs (applySep x s')

flat' :: [[String]] -> [String]
flat' [] = []
flat' [x] = x
flat' (x:xs) = x ++ (flat' xs)

split' :: String -> String -> [String]
split' sep str = conc' sep (splitOn sep str) 

conc' :: String -> [String] -> [String]
conc' _ [s] = [s]
conc' sep (s:ss) = s:sep:(conc' sep ss)

--TODO: Why can't i load this from normal Data.Either Package?
fromRight :: b -> Either a b -> b 
fromRight def (Right b) = b 
fromRight def (Left a) = def

safeRight = fromRight (Var "Err")

fromLeft :: a -> Either a b -> a 
fromLeft def (Right b) = def
fromLeft def (Left a) = a

safeLeft = fromLeft Minus
