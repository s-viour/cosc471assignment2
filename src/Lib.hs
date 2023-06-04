module Lib
  ( bisection
  , fixedPoint
  , newtonRaphson
  ) where

bisection' :: (Double -> Double) -> (Double, Double) -> (Double, Double)
bisection' f (a, b)
  | (f a) * (f c) < 0 = (a, c)
  | otherwise = (b, c)
  where c = (a + b) / 2

bisection :: (Double -> Double) -> (Double, Double) -> Int -> Double
bisection f ab n = (midpoint . last . take n) (iterate (bisection' f) ab)
  where midpoint (a, b) = (a + b) / 2

fixedPoint :: (Double -> Double) -> Double -> Int -> Double
fixedPoint f x n = (last . take n) (iterate f x)

newtonRaphson' :: (Double -> Double) -> (Double -> Double) -> Double -> Double
newtonRaphson' f f' x = x - ((f x) / (f' x))

newtonRaphson :: (Double -> Double) -> (Double -> Double) -> Double -> Int -> Double
newtonRaphson f f' x0 n = (last . take n) (iterate (newtonRaphson' f f') x0)