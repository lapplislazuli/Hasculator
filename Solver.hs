-- This File contains solving Terms
module Solver where

import Terms

type ErrDouble = Either String Double

fx :: Term -> Double -> Double
fx t v = solve t [("x",v)]

-- Sweeter Input and iterations bound to 250 
-- 250 is a-ok for most terminations and don't take that long
regulaFalsi :: Term -> Double -> Double -> ErrDouble
regulaFalsi t a b = failableRegulaFalsi t 250 (Right a) (Right b)

-- https://en.wikipedia.org/wiki/False_position_method
-- Params: Term (With x), Iterations , A, B 
-- Output: xKoord of 0-Point
failableRegulaFalsi :: Term -> Int -> ErrDouble -> ErrDouble -> ErrDouble
-- Somewhere i've reached invalid input - abort and pass error
failableRegulaFalsi _ _ (Left a) _ = Left a 
failableRegulaFalsi _ _ _ (Left b) = Left b 
-- Stop recursion - give me the value which is closer to 0
failableRegulaFalsi _ 1 (Right a) (Right b) 
    | abs a >  abs b = (Right b) 
    | abs b >= abs a = (Right a)
failableRegulaFalsi t n (Right a) (Right b)
    | a > b    = Left "InvalidInput - a < b required"
    | f a > 0 || f b < 0 = Left "InvalidInput - f(a) < 0 and f(b) > 0 required!"
    | f c <= 0 = failableRegulaFalsi t (n-1) (Right c)  (Right b)
    | f c >  0 = failableRegulaFalsi t (n-1) (Right a) (Right c) 
    where 
        f  = fx t
        c  = (f b * f a - f a * b) / ( f b - f a)