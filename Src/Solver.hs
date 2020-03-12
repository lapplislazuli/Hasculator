-- This File contains solving Terms
module Solver where

import Terms

type ErrDouble = Either String Double

fx :: Term -> Double -> Double
fx t v = solve t [("x",v)]

-- Sweeter Input and iterations bound to 250 
-- 250 is a-ok for most terminations and don't take that long
regulaFalsi250 :: Term -> Double -> Double -> ErrDouble
regulaFalsi250 t = failableRegulaFalsi t 250

failableRegulaFalsi :: Term -> Int -> Double -> Double -> ErrDouble
failableRegulaFalsi t i a b
    |   a > b    = Left "InvalidInput - a < b required"
    |   f a > 0 || f b < 0 = Left "InvalidInput - f(a) < 0 and f(b) > 0 required!"
    |   otherwise = Right (regulaFalsi f i a b) 
    where f  = fx t

-- https://en.wikipedia.org/wiki/False_position_method
-- Params: Term (With x), Iterations , A, B 
-- Output: xKoord of 0-Point
regulaFalsi :: (Double -> Double) -> Int -> Double -> Double -> Double
-- end-step: all iterations taken
regulaFalsi f 0 a b
    | abs (f a) >  abs (f b) = b
    | abs (f b) >= abs (f a) = a
-- recursive step
regulaFalsi f n a b
    | f c * f b > 0 = regulaFalsi f (n-1) c b
    | f c * f a > 0 = regulaFalsi f (n-1) a c
    | otherwise = regulaFalsi f 0 a c
    where 
        c  = (f a * b - f b * a) / ( f a - f b)