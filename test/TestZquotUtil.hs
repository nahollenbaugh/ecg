{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestZquotUtil (htf_thisModulesTests) where

import Test.Framework
import Test.Framework.TestInterface
import Zquot
import ZquotUtil
import Util

prop_isSquare :: Zquot -> Bool
prop_isSquare x = or (map (\y -> x == y * y) (enumerateZquot x)) == isSquare x
test_isSquare = assertBool $ not $ isSquare Undefined

prop_valuation_1 :: Integer -> Integer -> Bool
prop_valuation_1 0 _ = True
prop_valuation_1 x n = let p     = nextPrime ((abs n) + 1)
                           (e,q) = valuation x p
                       in q == pow p e
-- the p valuation of x is the power of p (prop 1: )dividing x and (prop 2: )whose
-- quotient with x is prime to p. 
prop_valuation_2 :: Integer -> Integer -> Bool
prop_valuation_2 0 _ = True
prop_valuation_2 x n = let p = nextPrime ((abs n))
                       in mod x (pow p (fst $ valuation x p)) == 0
prop_valuation_3 0 _ = True
prop_valuation_3 x n = let p = nextPrime ((abs n) + 1)
                       in mod (quot x (pow p (fst $ valuation x p))) p /= 0

prop_sqrtModp_1 :: Integer -> Integer -> Bool
prop_sqrtModp_1 m n = let x = Value (nextPrime (abs m)) n
                      in if isSquare x
                         then let y = sqrtModp x
                              in y * y == x
                         else prop_sqrtModp_1 m (n+1)

prop_intSqrt_1 :: Integer -> Bool
prop_intSqrt_1 n = let square = nextSquareInt $ abs n
                       sqrt = intSqrt square
                   in sqrt * sqrt == square
  where nextSquareInt n = let s = head $ filter (\x -> x * x >= n) [1..]
                           in s * s
prop_intSqrt_2 :: Integer -> Bool
prop_intSqrt_2 n
  | n < 0 = prop_intSqrt_2 (-n)
  | otherwise = let sqrt = intSqrt n
                in sqrt * sqrt <= n && (sqrt + 1) * (sqrt + 1) > n

-- wtf is this
prop_cornacchia_1 :: Integer -> Integer -> Bool
prop_cornacchia_1 m n = let p = nextPrime $ 3 + abs n
                            d = let d' = mod m p
                                in if d' == 0
                                   then 1
                                   else d'
                        in if isSquare $ Value p (-d)
                           then case cornacchia d p of
                                  Just (x,y) -> ((x * x) + (d * y * y) == p) || (and [x*x+d*y*y/=p | x<-[0..p-1], y<-[0..p-1]])
                                  Nothing    -> True
                                  -- Nothing    -> False
                           else prop_cornacchia_1 (m+1) n
prop_cornacchia_2 :: Integer -> Integer -> Bool
prop_cornacchia_2 m n = let p = nextPrime $ 3 + abs n
                            d = if mod m p == 0 then 1 else mod m p
                        in case cornacchia d p of
                             Nothing    -> null [1 | x <- [0..intSqrt p], y <- [0.. intSqrt p],  x*x + d*y*y == p]
                             Just (x,y) -> x*x + d*y*y == p
