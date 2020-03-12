module Tests.SolverTests (operatorTests,variableTests,solverTests) where 

import Tests.TestHelpers


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

testAdd = 
    3 ~=? simpleParseAndSolve "1 + 2"
testSub = 
    1 ~=? simpleParseAndSolve "2 - 1"
testMul = 
    6 ~=? simpleParseAndSolve "3 * 2"
testDiv = 
    5 ~=? simpleParseAndSolve "10 / 2"
testPow = 
    4 ~=? simpleParseAndSolve "2 ^ 2"
testPowLow = 
    2 ~=? simpleParseAndSolve "4 ^ ( 1 / 2 )"
testNegPow = 
    0.5 ~=? simpleParseAndSolve "2 ^ ( 0 - 1 ) "

--Exp was a little inconvenient, so I rounded it to 8 digits after
testExp = 
    truncate' (exp 3) 8 ~=? truncate' (simpleParseAndSolve "Exp 3") 8
testLn = 
    log 10 ~=? simpleParseAndSolve "Ln 10"

variableTests = TestList [
    TestLabel "Insert One Variable " testVar1
    ,TestLabel "Insert two Variables " testVar2
    ,TestLabel "One Missing Var" testOneMissing
    ,TestLabel "Too Many Vars" testOneTooMuch
    ,TestLabel "Too Many Vars II" testTooMuch
    ]

testVar1 = 
    1 ~=? parseAndSolve "x" [("x",1)]
testVar2 = 
    6 ~=? parseAndSolve "x * y" [("x",2),("y",3)]
testOneMissing = 
    2 ~=? parseAndSolve "x + y" [("x",2)]
testOneTooMuch = 
    3 ~=? parseAndSolve "x + y" [("x",2),("y",1),("z",2)]
testTooMuch = 
    5 ~=? parseAndSolve "5" [("x",2),("y",1),("z",2)]


solverTests = TestList [
    TestLabel "Regula Falsi x + 2 " testRegula1
    ,TestLabel "Regula Falsi x - 2 " testRegula2
    , TestLabel "Regula Falsi x + 3 " testRegula3
    , TestLabel "Regula Falsi x -3 " testRegula4
    , TestLabel "Regula Falsi 2x-20" testRegula5
    , TestLabel "Regula Falsi x" testRegula6
    , TestLabel "Regula Falsi x^3" testRegula7
    , TestLabel "Regula Falsi x*x*x" testRegula8
    , TestLabel "Regula Falsi x^5" testRegula9
    ,TestLabel "Regula Falsi x^3-1" testRegulaPol
    ,TestLabel "Regula Falsi x^3+1" testRegulaPol2
    ]

testRegula1 =  
    -2 ~=? round (regulaFalsi' (parse "x + 2") (-4) 4 )
testRegula2 =  
    2 ~=? round (regulaFalsi' (parse "x - 2") 1 5 )
testRegula3 =  
    -3 ~=? round (regulaFalsi' (parse "x + 3") (-5) 5 )
testRegula4 =  
    3 ~=? round (regulaFalsi' (parse "x - 3") (-5) 5 )
testRegula5 =  
    10 ~=? round (regulaFalsi' (parse "( x * 2) - 20") (-4) 24 )
testRegula6 =  
    0 ~=? round (regulaFalsi' (parse "x") (-4) 4 )
testRegula7 =  
    0 ~=? round (regulaFalsi' (parse "x^3 ") (-4) 4 )
testRegula8 =  
    0 ~=? round (regulaFalsi' (parse "x * x * x") (-4) 4 )
testRegula9 =  
    0 ~=? round (regulaFalsi' (parse "x^5") (-4) 4 )




testRegulaPol = 
    1 ~=? round (regulaFalsi' (parse "(x^3) - 1") (-2) 3 )
testRegulaPol2 = 
    -1 ~=? round (regulaFalsi' (parse "(x^3) + 1") (-4) 3 )

--testRegulaPol2 =  
--   0 ~=? round (regulaFalsi' (parse "(0-1)*x^3")  (-2) 1 )