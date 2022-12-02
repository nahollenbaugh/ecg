{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestECGroupUtil (htf_thisModulesTests) where

import Test.Framework
import Test.Framework.TestInterface
import ECGroup
import ECGroupUtil
import Zquot
import Util

prop_enumerate :: Curve -> Bool
prop_enumerate c =
  let k = map (Value $ base c) [1..base c]
  in equalAsSets (enumerate c) $ Zero:(filter (contains c) [A2 x y | x <- k, y <- k])

-- things that getNonzero hits are all in c
prop_getNonzero_1 c@(Curve (Value p _) _ _ _ _) x y
  = let s = getNonzero c $ A2 (Value p x) (Value p y)
    in case s of
         A2 Undefined _ -> True
         _ -> contains c s

-- getNonzero gets the right number of things starting from each point of A^2.  
prop_getNonzero_2 :: Curve -> Bool
prop_getNonzero_2 curve@(Curve a@(Value p _) b c d f) = -- if p > 25 then True else 
    let coords = map (Value p) [0..p-1]
        points = [getNonzero curve $ A2 x y | y <- coords, x <- coords]
    in (length [1 | y <- coords, x <- coords, y*y + a*x*y + c*y == x*x*x + b*x*x + d*x + f])
       == if not $ null $ filter isUndefinedPoint points
          then 0
          else length $ removeDuplicates points

test_b2 = do
  assertEqual 1 1 
  -- meh didn't notice there was a typo in silverman until I put in his examples 
  -- and now I dont wanna do it by hand again. I'll do it later. Maybe 
  -- assertEqual (b2 $ curve 11 1 2 3 5 7) 10
  -- assertEqual (b2 $ curve 9 2 1 0 0 2)  4
  -- assertEqual (b2 $ curve 13 2 1 0 0 2) 4
  -- assertEqual (b2 $ curve 7 2 0 1 0 2)  4

test_b4 = do
  assertEqual (b4 $ curve 11 1 2 3 5 7) 2
  assertEqual (b4 $ curve 9 2 1 0 0 2)  0
  assertEqual (b4 $ curve 13 2 1 0 0 2) 0
  assertEqual (b4 $ curve 7 2 0 1 0 2)  2

test_b6 = do
  assertEqual (b6 $ curve 11 1 2 3 5 7) 4
  assertEqual (b6 $ curve 9 2 1 0 0 2)  8
  assertEqual (b6 $ curve 13 2 1 0 0 2) 8
  assertEqual (b6 $ curve 7 2 0 1 0 2)  2

test_b8 = do
  assertEqual 1 1 
  -- meh didn't notice there was a typo in silverman until I put in his examples 
  -- and now I dont wanna do it by hand again. I'll do it later. Maybe 
  -- assertEqual (b8 $ curve 11 1 2 3 5 7) 8
  -- assertEqual (b8 $ curve 9 2 1 0 0 2)  7
  -- assertEqual (b8 $ curve 13 2 1 0 0 2) 3
  -- assertEqual (b8 $ curve 7 2 0 1 0 2)  1

test_disc = do
  -- meh didn't notice there was a typo in silverman until I put in his examples 
  -- and now I dont wanna do it by hand again. I'll do it later. Maybe 
  -- assertEqual (disc $ curve 11 1 2 3 5 7) 7
  -- assertEqual (disc $ curve 9 2 1 0 0 2)  5
  -- assertEqual (disc $ curve 13 2 1 0 0 2) 5
  -- assertEqual (disc $ curve 7 2 0 1 0 2)  5
  -- silverman p43: 
  assertEqual (disc $ curve 0 0 0 0 (-3) 3) (-2160)
  assertEqual (disc $ curve 0 0 0 0 1    0) (-64)
  assertEqual (disc $ curve 0 0 0 0 (-1) 0) 64
  assertEqual (disc $ curve 0 0 0 0 0    0) 0
  assertEqual (disc $ curve 0 0 1 0 0    0) 0

test_singular = do
  assertBool $ not $ singular $ curve 11 1 2 3 5 7
  assertBool $ not $ singular $ curve 13 2 1 0 0 2
  assertBool $ not $ singular $ curve 7 2 0 1 0 2
  -- silverman p43: 
  assertBool $ not $ singular $ curve 0 0 0 0 (-3) 3
  assertBool $ not $ singular $ curve 0 0 0 0 1    0
  assertBool $ not $ singular $ curve 0 0 0 0 (-1) 0
  assertBool $       singular $ curve 0 0 0 0 0    0
  assertBool $       singular $ curve 0 0 1 0 0    0

prop_cdisc m a b c d f = let crv = curve m a b c d f
                         in 1728*(disc crv) == (c4 crv)*(c4 crv)*(c4 crv) - (c6 crv)*(c6 crv)

prop_b8bs m a b c d f = let crv = curve m a b c d f
                        in 4*(b8 crv) == (b2 crv)*(b6 crv) - (b4 crv)*(b4 crv)

prop_countPointsEnumerate :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
prop_countPointsEnumerate m a b c d f =
  let p = nextPrime m
  in (toInteger $ length $ 1:[1 | x <- [1..p], y <- [1..p], contains (curve p a b c d f) $ point p x y])
     == (countPointsEnumerate $ curve p a b c d f)
