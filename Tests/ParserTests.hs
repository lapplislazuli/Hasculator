module Tests.ParserTests (
    coreParserTests,
    tokenizerTests
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