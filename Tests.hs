import Test.HUnit
import Terms 
import Parser
import Solver
import Differentiator 

import Control.Exception

allTests = TestList [
     TestLabel "ParserTests" coreParserTests
    ,TestLabel "OperatorTests" operatorTests
    ,TestLabel "TokenizerTests" tokenizerTests
    ,TestLabel "Basic Show-Tests" termShowTests
    ,TestLabel "Reparse Tests" parseShowTests
    ,TestLabel "PrecedenceTests" precedenceTests
    ,TestLabel "NegationTests" negaterTests
    ,TestLabel "VariableTests" variableTests
    ,TestLabel "Basic DifferentiatorTests" coreDifferTests
    ,TestLabel "SolverTests" solverTests --TODO: These sometimes do not stop?
    ,TestLabel "SpecialTests" specialTests
    ]
    

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
    ]

--Simple Parsing
parSingleNumb = 1 ~=? (simParSolv "1")
parSingleNumb2 =  (simParSolv "1") ~=? 1 
parSingleVar  = 0 ~=? (simParSolv "a")
parLongerVar  = 0 ~=? (simParSolv "long")
parConst      = exp(1) ~=? (simParSolv "e")
parConstExtra = 0 ~=? (simParSolv "ee")

parBracketedNumb = 1 ~=? (simParSolv "( 1 )")
parDoubleBracketedNumb = 1 ~=? (simParSolv "( ( 1 ) )")

-- ErrorHandling
lostBracket = ErrorTerm "LostClosingBracketErr" ~=? parse "1 )"
lostBracket2 = ErrorTerm "LostClosingBracketErr" ~=? parse "1 ))"
lostBracket3 = ErrorTerm "LostClosingBracketErr" ~=? parse "( 1 ) + a )"
tooManyTerms = ErrorTerm "MissingOperatorErr" ~=? parse "1 a" 
missingArgs = ErrorTerm "LostOperatorErr" ~=? parse "+"
emptyTerm = ErrorTerm "EmptyTermErr" ~=? parse " "
emptyBrackets = ErrorTerm "SafeRightErr" ~=? parse "( )"

openBrackets1 = ErrorTerm "LostOpeningBracketErr" ~=? parse "( a + b "
openBrackets3 = Add (Numb 1) (ErrorTerm "LostOpeningBracketErr") ~=? parse "( 1 ) + ( 4"
openBrackets2 = ErrorTerm "LostOpeningBracketErr" ~=? parse "( ( 1 )"
openBrackets4 = ErrorTerm "LostOpeningBracketErr" ~=? parse "( ( ( 1 "
openBrackets5 = ErrorTerm "LostOpeningBracketErr" ~=? parse "( 1 "

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

testNsp1 = 3 ~=? (simParSolv "1+2")
testNsp2 = 10 ~=? (simParSolv "(1+2*4)+1")
testNsp3 = 16 ~=? (simParSolv "3*2+2*5")
testMsp1 = 3 ~=? (simParSolv "1 +2")
testMsp2 = 10 ~=? (simParSolv "(1+ 2* 4)+1 ")
testFsp1 = 6 ~=? (simParSolv " 3 * 2 ")
testOfsp1 = 10 ~=? (simParSolv "(1  + 2*  4) +1   ")
testOfsp2 = 10 ~=? (simParSolv "   (    1+    2* 4   )    +1 ")

operatorTests = TestList [
    TestLabel "Addition" testAdd 
    ,TestLabel "Substraction" testSub
    ,TestLabel "Multiplication" testMul 
    ,TestLabel "Divion" testDiv 
    ,TestLabel "NaturalLog" testLn
    ,TestLabel "Power" testPow
    ,TestLabel "SmallerPow" testPowLow
    ,TestLabel "NegativePow" testNegPow
    ,TestLabel "Exp" testExp 
    ]

testAdd = 3 ~=? (simParSolv "1 + 2")
testSub = 1 ~=? (simParSolv "2 - 1")
testMul = 6 ~=? (simParSolv "3 * 2")
testDiv = 5 ~=? (simParSolv "10 / 2")
testPow = 4 ~=? (simParSolv "2 ^ 2")
testPowLow = 2 ~=? (simParSolv "4 ^ ( 1 / 2 )")
testNegPow = 0.5 ~=? (simParSolv "2 ^ ( 0 - 1 ) ")
--Exp was a little Bitchy, so i rounded it to 8 digits after
testExp = truncate' (exp 3) 8 ~=? truncate' (simParSolv "Exp 3") 8
testLn = log 10 ~=? (simParSolv "Ln 10")


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
    
    ]

testPS = 5 ~=? (simParSolv "1 + 2 * 2")
testPP = 12 ~=? (simParSolv "3 * 2 ^ 2 ")
testFPoi = (truncate' ((exp 3) * 3) 8) ~=? truncate' (simParSolv "Exp 3 * 3") 8
testFPow = (truncate' ((exp 3) ** 2) 8) ~=? truncate' (simParSolv "Exp 3 ^ 2 ") 8
testBU = truncate' (exp 3) 8 ~=? truncate' (simParSolv "Exp ( 1 + 2 )") 8
testBB = 6 ~=? (simParSolv "( 1 + 1 ) * 3")
testSES = 3 ~=? (simParSolv "2 + 2 - 1")
testPEP = 8 ~=? (simParSolv "( 1 * 2 ) + ( 2 * 3 )")

testNegSt = 1 ~=? simParSolv "!1+2"
testNegPt = (-6) ~=? simParSolv "!2*3"
testNegFn1 = ErrorTerm "MissingOperatorErr" ~=? parse "! Exp 0"
testNegFn2 = ErrorTerm "MissingOperatorErr" ~=? parse "Exp ! 0.5" 

variableTests = TestList [
    TestLabel "Insert One Variable " testVar1
    ,TestLabel "Insert two Variables " testVar2
    ,TestLabel "One Missing Var" testOneMissing
    ,TestLabel "Too Many Vars" testOneTooMuch
    ,TestLabel "Too Many Vars II" testTooMuch
    ]

testVar1 = 1 ~=? (parSolv "x" [("x",1)])
testVar2 = 6 ~=? (parSolv "x * y" [("x",2),("y",3)])
testOneMissing = 2 ~=? (parSolv "x + y" [("x",2)])
testOneTooMuch = 3 ~=? (parSolv "x + y" [("x",2),("y",1),("z",2)])
testTooMuch = 5 ~=? (parSolv "5" [("x",2),("y",1),("z",2)])

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

negPosNum = (-5) ~=? simParSolv "!5"
negNegNum = 5   ~=? simParSolv "!(0-5)"
negnegNum = 5   ~=? simParSolv "!(!5)"
negVar    = (-3)   ~=? parSolv "!a" [("a",3)]
negnegVar    = 3   ~=? parSolv "!(!a)" [("a",3)]
negBrackets = (-4) ~=? simParSolv "!(4)"
negZero = 0 ~=? simParSolv "!0"

negAdd = (-3) ~=? simParSolv "!(1+2)"
negSub = 3 ~=? simParSolv "!(1-4)"
negMul = (-6) ~=? simParSolv "!(2*3)"
negFn1 = (-1) ~=? simParSolv "!(Exp 0)" 
negFn2 = (-1) ~=? simParSolv "!(Ln e)"

termShowTests = TestList [
        TestLabel "Show Number" showNb
        ,TestLabel "Show Var" showVar
        ,TestLabel "Show longerVar" showLongVar
        ,TestLabel "Show NumberVar" showNumVar
        ,TestLabel "Show Err" showErr 
        ,TestLabel "Show Const" showConst

        ,TestLabel "Show Binary" showBinary
        ,TestLabel "Show Unary" showUnary
    ]

showNb = "2.0" ~=? show (Numb 2)
showVar = "a" ~=? show (Var "a")
showLongVar = "aTTbbhiaejoah" ~=? show (Var "aTTbbhiaejoah")
showNumVar = "a22" ~=? show (Var "a22")
showConst ="e" ~=? show (Const "e")
showErr = "TestErr" ~=? show (ErrorTerm "TestErr")

showBinary = "(a+b)"  ~=? show (Add (Var "a") (Var "b") )
showUnary = "Ln(b)"  ~=? show (Ln (Var "b") )

parseShowTests = TestList [
    TestLabel "Reparse Numb" repNum
    ,TestLabel "Reparse Var" repVar
    ,TestLabel "Reparse Const" repConst
    
    ,TestLabel "Reparse Addition" repAdd
    ,TestLabel "Reparse Sub" repSub
    ,TestLabel "Reparse Mul" repMul 
    ,TestLabel "Reparse Div" repDiv
    ,TestLabel "Reparse Ln" repLn 
    ,TestLabel "Reparse Exp" repExp 

    ,TestLabel "Reparse unary around binary" repUnBin
    ,TestLabel "Reparse binary around Unary" repBinUn
    ,TestLabel "Reparse binary around binary" repBinBin
    ,TestLabel "Reparse unary around unary" repUnUn
    ]

repNum = (Numb 2) ~=? reparse (Numb 2)
repVar = (Var "a") ~=? reparse (Var "a")
repConst = (Const "e") ~=? reparse (Const "e")

repAdd = (Add (Numb 2) (Var "a")) ~=? reparse (Add (Numb 2) (Var "a")) 
repSub = (Sub (Numb 2) (Var "a")) ~=? reparse (Sub (Numb 2) (Var "a"))
repMul = (Mul (Numb 2) (Var "a")) ~=? reparse (Mul (Numb 2) (Var "a"))
repDiv = (Div (Numb 2) (Var "a")) ~=? reparse (Div (Numb 2) (Var "a"))
repLn = (Ln (Var "a")) ~=? reparse (Ln (Var "a"))
repExp = (Exp (Var "a")) ~=? reparse (Exp (Var "a"))

repUnUn = (Exp (Exp (Var "a"))) ~=? reparse (Exp (Exp (Var "a"))) 
repUnBin = (Exp (Add (Var "a")(Numb 2))) ~=? reparse (Exp (Add (Var "a")(Numb 2)))
repBinBin = (Add (Add (Var "a") (Numb 1)) (Sub (Var "b")(Numb 1))) ~=? reparse (Add (Add (Var "a") (Numb 1)) (Sub (Var "b")(Numb 1)))
repBinUn = (Add (Ln (Var "a")) (Exp (Var "b"))) ~=? reparse (Add (Ln (Var "a")) (Exp (Var "b")))


solverTests = TestList [
    TestLabel "Regula Falsi I " testRegula1
    ,TestLabel "Regula Falsi II " testRegula2
    ,TestLabel "Regula Falsi Square" testRegulaSqr
 --   ,TestLabel "Regula Wrong input" testRegulaWIP --TODO: This one does not terminate???
    ,TestLabel "Regula Falsi x^3" testRegulaPol
--    ,TestLabel "Regula Falsi -x^3" testRegulaPol2 --TODO: This one Errors
    ]

testRegula1 =  (-2) ~=? round (regulaFalsi' (parse "x + 2") (-4) 4 )
testRegula2 =  (2) ~=? round (regulaFalsi' (parse "x - 2") (-5) 5 ) --TODO: Something is Wrong here!
testRegulaPol = (1) ~=? round (regulaFalsi' (parse "x^3 -1") (0) 2 )
testRegulaPol2 =  0 ~=? round (regulaFalsi' (parse "(0-1)*x^3")  (-2) 1 )
testRegulaSqr =  Left "InvalidInput - a < b required" ~=? (regulaFalsi (parse "x**2 + 2") (10) (-10) )
testRegulaWIP =  Left "InvalidInput - f(a) < 0 and f(b) > 0 required!" ~=? (regulaFalsi (parse "x**2 + 2") (-10) (10) )

coreDifferTests = TestList [
    TestLabel "No Vars I " testDFVar1
    ,TestLabel "No Vars II" testDFVar2
    ,TestLabel "Linear I" testDfLin1
    ,TestLabel "Linear II" testDfLin2
    ,TestLabel "Linear III" testDfLin3
    ,TestLabel "Linear Wrong Var" testLinWV
    
    ,TestLabel "Constant I"     testDFConst1
    ,TestLabel "Constant II"    testDFConst2
    ,TestLabel "Constant III"   testDFConst3
    ,TestLabel "Constant IV"    testDFConst4
    ,TestLabel "Constant V"     testDFConst5

    ,TestLabel "Simple Exp I" testDfExp1
    ,TestLabel "Simple Exp II" testDfExp2
    ,TestLabel "Simple Ln I" testDfLn1
    ,TestLabel "Simple Ln II" testDfLn2 
    ,TestLabel "Simple Ln III" testDfLn3 
    ]

testDFVar1 = 0 ~=? simpSolDifPar "2"
testDFVar2 = 0 ~=? simpSolDifPar "2*4 + 2 + 4*2"
testDfLin1 = 1 ~=? simpSolDifPar "x"
testDfLin2 = 1 ~=? simpSolDifPar "x+2"
testDfLin3 = (-1) ~=? simpSolDifPar "2-x"
testLinWV = 0 ~=? simpSolDifPar "y"

testDFConst1 = 0 ~=? simpSolDifPar "e"
testDFConst2 = 1 ~=? simpSolDifPar "e+x"
testDFConst3 = (truncate' (exp 1) 6) ~=? (truncate' (simpSolDifPar "e*x") 6)
testDFConst4 = 0 ~=? simpSolDifPar "e+e"
testDFConst5 = 0 ~=? simpSolDifPar "e*e"

testDfExp1 = 0 ~=? truncate' (simpSolDifPar "Exp 1") 8
testDfExp2 = truncate' ((exp 2)) 8 ~=? truncate' (solDifPar "Exp x" [("x",2.0)]) 8
testDfLn1 = 0 ~=? simpSolDifPar "Ln 3"
testDfLn2 = 1 ~=? solDifPar "Ln x" [("x",1.0)]
testDfLn3 = 0.5 ~=? solDifPar "Ln x" [("x",2.0)]

extendedDifferTests = TestList [
    TestLabel "Coefficient I" testDFCoeff1
    ,TestLabel "Coefficient II" testDFCoeff2
    ,TestLabel "Coefficient III" testDFCoeff3

    ,TestLabel "Pow I" testDFPow1
    ,TestLabel "Pow II" testDFPow2
    ,TestLabel "Pow III" testDFPow3
    ,TestLabel "Pow IV" testDFPow4
    ,TestLabel "Pow V" testDFPow5

    ,TestLabel "Div I" testDFDif1
    ,TestLabel "Div II" testDFDif2
    ,TestLabel "Div III" testDFDif3
    ]

testDFCoeff1 = 2 ~=? simpSolDifPar "2 * x"
testDFCoeff2 = (-1) ~=? simpSolDifPar "(0-1) * x"
testDFCoeff3 = 2 ~=? solDifPar "a * x" [("a",2.0)] 
testDFPow1 = 4 ~=? solDifPar "x^2" [("x",2.0)]
testDFPow2 = 12 ~=? solDifPar "x^3" [("x",2.0)]
testDFPow3 = 12 ~=? solDifPar "x^a" [("x",2.0),("a",3)]
testDFPow4 = 0 ~=? simpSolDifPar "a^5"
testDFPow5 = 0 ~=? simpSolDifPar "2^5"

testDFDif1 = -0.25 ~=? solDifPar "1/x" [("x",2.0)]
testDFDif2 = truncate' (-(0.5)) 6 ~=? truncate' (solDifPar "1/(2*x)" [("x",1.0)]) 6
testDFDif3 = truncate' (-(0.125)) 6 ~=? truncate' (solDifPar "1/(2*x)" [("x",2.0)]) 6
-- These Special Testcases originate from special bugs i've encountered and worked through,
-- They should be therefore checked forever after so i will hopefully never see them again
specialTests = TestList [
     TestLabel "Greedy Functions I " testGF1
    ,TestLabel "Greedy Functions II " testGF2
    ,TestLabel "Unnecessary Brackets" testUnBr
    ,TestLabel "Long Lost Bracket" testLongLostBR
    ,TestLabel "Long Lost opening Bracket" testLongLostBR2
    ]

-- All Originate from problems with the brackets
-- The first two usually resolved as follows ( Exp (1 + 2 ) + 3 ) = Exp 6 (Which is wrong btw)
testGF1 = (truncate' ((exp 3) + 3) 8) ~=? truncate' (simParSolv "( Exp ( 1 + 2 ) + 3 )") 8
testGF2 = (truncate' ((log 3) * 3) 8) ~=? truncate' (simParSolv "( ( Ln ( 1 + 2 ) ) * 3 ) ") 8
testUnBr = 5 ~=? simParSolv "( ( 1 + ( 2 ) ) ) + ( 1 + ( ( 1 ) + ( 0 ) ) )"
testLongLostBR = ErrorTerm "LostClosingBracketErr" ~=? parse "( ( ( 1 + ( 1 + 0 ) ) ) + ( 2 ) ) )"
testLongLostBR2 = ErrorTerm "LostOpeningBracketErr" ~=? parse "( ( ( ( ( 1 + ( 1 + 0 ) ) ) + ( 2 ) ) )"

------------------------------------
-- Helpers
------------------------------------ 

simpSolDifPar s = solDifPar s []
solDifPar s vars = solve ((dx . parse) s) vars 

--These methods shorthand some tests
parSolv :: String -> [(String, Double)] -> Double
parSolv str vars = solve (parse str) vars
--Solves the Term for no-Variables (or every Variable 0)
simParSolv :: String -> Double 
simParSolv str = (parSolv str []) 

reparse :: Term -> Term 
reparse = parse . show

regulaFalsi' :: Term -> Double -> Double -> Double
regulaFalsi' t a b = removeErrDouble (regulaFalsi t a b)

removeErrDouble :: Either String Double -> Double
removeErrDouble (Right d) = d
removeErrDouble (Left e) = error e 

truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n