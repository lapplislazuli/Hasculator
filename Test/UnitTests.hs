module Main where

import Tests.ParserTests
import Tests.ReadNShowTests
import Tests.DifferTests 
import Tests.SolverTests
import Tests.TestHelpers

import Test.Framework
import Test.Framework.Providers.HUnit

tests = hUnitTestToTests allTests

main = defaultMain tests

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
