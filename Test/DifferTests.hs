module Tests.DifferTests (coreDifferTests,extendedDifferTests) where 

import Tests.TestHelpers

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