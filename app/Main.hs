module Main (main) where

import Lib ( bisection, fixedPoint, newtonRaphson )

getF :: Double -> Double
getF x = sin (1 / x**2)

getG :: Double -> Double
getG x = getF x + x

getFPrime :: Double -> Double
getFPrime x = cos (1 / x**2) * (-2 / x**3)

main :: IO ()
main = do
  putStrLn $ "25 iterations of the bisection method yields: " ++ show bres
  putStrLn $ "25 iterations of the newton-raphson method yields: " ++ show nres
  putStrLn $ "250 iterations of the fixed-point iteration method yields: " ++ show ires
  where
    bres = bisection getF (0.5, 1) 25
    nres = newtonRaphson getF getFPrime (3/5) 25
    ires = fixedPoint getG (3/5) 250

-- differentiating 1/x^2
-- x^-2
-- -2 * x^-3
--
