module Tests.ParserTests (
    coreParserTests,
    tokenizerTests,
    precedenceTests,
    negaterTests
)where 

import Tests.TestHelpers

coreParserTests = TestList [
    TestLabel "Single Number Parsing" parSingleNumb 
   ,TestLabel "Single Variable Parsing" parSingleVar
   ,TestLabel "Longer Variable Parsing" parLongerVar
   ,TestLabel "SingleBrackets" parBracketedNumb
   ,TestLabel "DoubleBrackets" parDoubleBracketedNumb
   ,TestLabel "Constant" parConst
   ,TestLabel "NoConst" parConstExtra

   ,TestLabel "Lost Bracket" lostBracket
   ,TestLabel "Multiple Lost Bracket" lostBracket2
   ,TestLabel "Lost second Bracket" lostBracket3
   ,TestLabel "Missing Operator" tooManyTerms
   ,TestLabel "Missing Arguments" missingArgs
   ,TestLabel "Empty Term" emptyTerm
   ,TestLabel "Empty Term (Empty Brackets)" emptyBrackets

   ,TestLabel "Lost opening Bracket I" openBrackets1
   ,TestLabel "Lost nested Opening Brackets" openBrackets2
   ,TestLabel "Lost second Opening Brackets" openBrackets3
   ,TestLabel "Multiple lost open Brackets" openBrackets4
   ,TestLabel "Open Br -> Term" openBrackets5

   ,TestLabel "Sinus parsed" parSins
   ,TestLabel "Cosinus parsed" parCos
   ]

--Simple Parsing
parSingleNumb = 
    1 ~=? simpleParseAndSolve "1"
parSingleNumb2 =  
    simpleParseAndSolve "1" ~=? 1
parSingleVar  = 
    0 ~=? simpleParseAndSolve "a"
parLongerVar  = 
    0 ~=? simpleParseAndSolve "long"
parConst      = 
    exp 1 ~=? simpleParseAndSolve "e"
parConstExtra = 
    0 ~=? simpleParseAndSolve "ee"

parBracketedNumb = 
    1 ~=? simpleParseAndSolve "( 1 )"
parDoubleBracketedNumb = 
    1 ~=? simpleParseAndSolve "( ( 1 ) )"

-- ErrorHandling
lostBracket = 
    ErrorTerm "LostClosingBracketErr" ~=? parse "1 )"
lostBracket2 = 
    ErrorTerm "LostClosingBracketErr" ~=? parse "1 ))"
lostBracket3 = 
    ErrorTerm "LostClosingBracketErr" ~=? parse "( 1 ) + a )"
tooManyTerms = 
    ErrorTerm "MissingOperatorErr" ~=? parse "1 a" 
missingArgs = 
    ErrorTerm "LostOperatorErr" ~=? parse "+"
emptyTerm = 
    ErrorTerm "EmptyTermErr" ~=? parse " "
emptyBrackets = 
    ErrorTerm "SafeRightErr" ~=? parse "( )"

openBrackets1 = 
    ErrorTerm "LostOpeningBracketErr" ~=? parse "( a + b "
openBrackets3 = 
    Add (Numb 1) (ErrorTerm "LostOpeningBracketErr") ~=? parse "( 1 ) + ( 4"
openBrackets2 = 
    ErrorTerm "LostOpeningBracketErr" ~=? parse "( ( 1 )"
openBrackets4 = 
    ErrorTerm "LostOpeningBracketErr" ~=? parse "( ( ( 1 "
openBrackets5 = 
    ErrorTerm "LostOpeningBracketErr" ~=? parse "( 1 "

parSins = 
    Sin (Numb 3) ~=? parse "Sin 3"
parCos =
    Cos (Numb 3) ~=? parse "Cos 3"


tokenizerTests = TestList [
   TestLabel "NoSpaces I" testNsp1
   ,TestLabel "NoSpaces II" testNsp2
   ,TestLabel "NoSpaces III" testNsp3
   ,TestLabel "MixedSpaces I" testMsp1
   ,TestLabel "MixedSpaces II" testMsp2
   ,TestLabel "FullSpaces I" testFsp1
   ,TestLabel "Overfull Spaces I" testOfsp1
   ,TestLabel "Overfull Spaces II" testOfsp2
   ]

testNsp1 = 
    3 ~=? simpleParseAndSolve "1+2"
testNsp2 = 
    10 ~=? simpleParseAndSolve "(1+2*4)+1"
testNsp3 = 
    16 ~=? simpleParseAndSolve "3*2+2*5"
testMsp1 = 
    3 ~=? simpleParseAndSolve "1 +2"
testMsp2 = 
    10 ~=? simpleParseAndSolve "(1+ 2* 4)+1 "
testFsp1 = 
    6 ~=? simpleParseAndSolve " 3 * 2 "
testOfsp1 = 
    10 ~=? simpleParseAndSolve "(1  + 2*  4) +1   "
testOfsp2 = 
    10 ~=? simpleParseAndSolve "   (    1+    2* 4   )    +1 "



precedenceTests = TestList [
    TestLabel " Point > Stick"      testPS
    ,TestLabel " Power > Point"     testPP
    ,TestLabel "Fns > Point"        testFPoi
    ,TestLabel "Fns > Power"        testFPow
    ,TestLabel "Brackets Unary"     testBU
    ,TestLabel "Brackets Binary"    testBB
    ,TestLabel "Stick = Stick"      testSES
    ,TestLabel "Power = Power"      testPEP
    
    ,TestLabel "Negate > Stick"      testNegSt
    ,TestLabel "Negate > Point"      testNegPt
    ,TestLabel "Negate = Fns I "      testNegFn1
    ,TestLabel "Negate = Fns II"      testNegFn2

    ,TestLabel "Sinus stronger than +" testSinPlus
    ]

testPS = 
    5 ~=? simpleParseAndSolve "1 + 2 * 2"
testPP = 
    12 ~=? simpleParseAndSolve "3 * 2 ^ 2 "
testFPoi = 
    truncate' (exp 3 * 3) 8 ~=? truncate' (simpleParseAndSolve "Exp 3 * 3") 8
testFPow = 
    truncate' (exp 3 ** 2) 8 ~=? truncate' (simpleParseAndSolve "Exp 3 ^ 2 ") 8
testBU = 
    truncate' (exp 3) 8 ~=? truncate' (simpleParseAndSolve "Exp ( 1 + 2 )") 8
testBB = 
    6 ~=? simpleParseAndSolve "( 1 + 1 ) * 3"
testSES = 
    3 ~=? simpleParseAndSolve "2 + 2 - 1"
testPEP = 
    8 ~=? simpleParseAndSolve "( 1 * 2 ) + ( 2 * 3 )"

testNegSt = 
    1 ~=? simpleParseAndSolve "!1+2"
testNegPt = 
    (-6) ~=? simpleParseAndSolve "!2*3"
testNegFn1 = 
    ErrorTerm "MissingOperatorErr" ~=? parse "! Exp 0"
testNegFn2 = 
    ErrorTerm "MissingOperatorErr" ~=? parse "Exp ! 0.5" 

testSinPlus = 
    Add (Sin (Numb 3)) (Numb 3) ~=? parse "Sin 3 + 3"

negaterTests = TestList [
    TestLabel "Negate Positive Number" negPosNum
    ,TestLabel "Negate Negative Number" negNegNum
    ,TestLabel "Double Negate Number"negnegNum
    ,TestLabel "Negate Var" negVar 
    ,TestLabel "DoubleNegate Var" negnegVar
    ,TestLabel "NegateBrackets" negBrackets
    ,TestLabel "Negate Zero" negZero

    ,TestLabel "Negate Addition I" negAdd
    ,TestLabel "Negate Addition II" negSub
    ,TestLabel "Negate Mult" negMul 
    ,TestLabel "Negate Fn I" negFn1
    ,TestLabel "Negate Fn II" negFn2
    ]

negPosNum = 
    (-5) ~=? simpleParseAndSolve "!5"
negNegNum = 
    5 ~=? simpleParseAndSolve "!(0-5)"
negnegNum = 
    5 ~=? simpleParseAndSolve "!(!5)"
negVar = 
    (-3) ~=? parseAndSolve "!a" [("a",3)]
negnegVar =
    3 ~=? parseAndSolve "!(!a)" [("a",3)]
negBrackets = 
    (-4) ~=? simpleParseAndSolve "!(4)"
negZero = 
    0 ~=? simpleParseAndSolve "!0"

negAdd = 
    (-3) ~=? simpleParseAndSolve "!(1+2)"
negSub = 
    3 ~=? simpleParseAndSolve "!(1-4)"
negMul = 
    (-6) ~=? simpleParseAndSolve "!(2*3)"
negFn1 =
    (-1) ~=? simpleParseAndSolve "!(Exp 0)" 
negFn2 = 
    (-1) ~=? simpleParseAndSolve "!(Ln e)"