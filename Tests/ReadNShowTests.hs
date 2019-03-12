module Tests.ReadNShowTests (termShowTests,parseShowTests) where 

import Tests.TestHelpers

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