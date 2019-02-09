-- This File contains solving Terms
module Solver where

import Terms

fx :: Term -> Double -> Double
fx t v = solve t [("x",v)]

-- https://en.wikipedia.org/wiki/False_position_method
-- Params: Term (With x), Iterations , A, B 
-- Output: xKoord of 0-Point
regulaFalsi :: Term -> Int ->  Double -> Double -> Double
-- Stop recursion - give me the value which is closer to 0
regulaFalsi _ 1 a b 
    | abs a >  abs b = b 
    | abs b >= abs a = a
regulaFalsi t n a b
    | a > b    = error "InvalidInput - a < b required"
    | f a > 0 || f b < 0 = error "InvalidInput - f(a) < 0 and f(b) > 0 required!"
    | f c <= 0 = regulaFalsi t (n-1) c b
    | f c >  0 = regulaFalsi t (n-1) a c 
    where 
        f  = fx t
        c  = (f b * f a - f a * b) / ( f b - f a)