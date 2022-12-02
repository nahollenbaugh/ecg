{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestZquot (htf_thisModulesTests) where

import Test.Framework
import Test.Framework.TestInterface
import Zquot
import Functions
import Util

prop_pow_1 :: Bool -> Zquot -> Int -> Bool
prop_pow_1 True = prop_pow_1 False . bump 
prop_pow_1 False = \x@(Value p a) n ->  if (n < 0)
                                        then if 1 == (gcd p a)
                                             then prop_pow_1 False ((Value p 1) / x) (-n)
                                             else True
                                        else (foldl (*) 1 (take n $ repeat x)) == (pow x n)

test_isZero = do
  assertBool $ not $ isZero (Value 0 1)
  assertBool $       isZero (Value 0 0)
  assertBool $       isZero (Value 3 0)
  assertBool $       isZero (Value 3 6)
  assertBool $ not $ isZero (Value 3 5)
  assertBool $ not $ isZero Undefined

test_isOne = do
  assertBool $       isOne (Value 0 1)
  assertBool $ not $ isOne (Value 0 0)
  assertBool $       isOne (Value 3 1)
  assertBool $       isOne (Value 3 7)
  assertBool $ not $ isOne (Value 3 5)
  assertBool $ not $ isOne Undefined

test_equality = do
  assertBool $ (Value 3 5)==(Value 3 2)
  assertBool $ (Value 3 2)/=(Value 2 2)
  assertBool $ (Value 0 2)==(Value 3 2)
  assertBool $ (Value 3 2)/= Undefined
  assertBool $ (Value 0 2)==(Value 2 4)
  assertBool $ (Value 0 2)==(Value 0 2)
  assertBool $ (Value 0 5)==(Value 3 2)

test_add = do
  assertEqual ((Value 7 (-11)) + (Value 7 4)) (Value 7 0)
  assertEqual ((Value 7 0) + (Value 7 2))     (Value 7 2)
  assertEqual ((Value 0 3) + (Value 0 (-3)))  (Value 0 0)
  assertEqual ((Value 0 0) + (Value 0 2))     (Value 0 2)
  assertBool $ isUndefined ((Value 7 3) + Undefined)
  assertBool $ isUndefined (Undefined + (Value 7 2))
  assertBool $ isUndefined ((Value 0 3) + Undefined)
  assertBool $ isUndefined (Undefined + (Value 0 2))

test_negative = do
  assertEqual (-(Value 3 4)) (Value 3 2)
  assertEqual (-(Value 0 5)) (Value 0 (-5))
  assertBool $ isUndefined (-Undefined)
prop_negative_1 :: Bool -> Zquot -> Bool
prop_negative_1 True = prop_negative_1 False . bump 
prop_negative_1 False = \x -> x - x == 0

test_multiply = do
  assertEqual ((Value 11 2) * (Value 11 7))  (Value 11 3)
  assertEqual ((Value 11 11) * (Value 11 8)) (Value 11 0)
  assertEqual ((Value 0 3) * (Value 0 2))    (Value 0 6)
  assertBool $ isUndefined ((Value 3 8) * (Value 7 2))
  assertBool $ isUndefined ((Value 0 3) * Undefined)
  assertBool $ isUndefined (Undefined * (Value 7 2))
  assertBool $ isUndefined (Undefined * Undefined)
prop_multiply_1 :: Bool -> Zquot -> Zquot -> Bool
prop_multiply_1 True = on (prop_multiply_1 False) bump
prop_multiply_1 False = \x y -> (x * y) == Value 0 ((toLift x) * (toLift y))

test_recip = do
  assertEqual (recip (Value 11 3))   (Value 11 4)
  assertEqual (recip (Value 3 (-1))) (Value 3 2)
  assertEqual (recip (Value 0 1))    (Value 0 1)
  assertEqual (recip (Value 0 (-1))) (Value 0 (-1))
  assertBool $ isUndefined (recip (Value 0 2))
  assertBool $ isUndefined (recip (Value 7 0))
  assertBool $ isUndefined (recip (Value 6 2))
  assertBool $ isUndefined (recip Undefined)
prop_recip_9 :: Bool -> Zquot -> Bool
prop_recip_9 True = prop_recip_9 False . bump 
prop_recip_9 False = \x@(Value p a) -> if 1 == (gcd a p)
                                       then x * (recip x) == 1
                                       else isUndefined (recip x)

prop_ring_1 :: Bool -> Zquot -> Zquot -> Zquot -> Bool
prop_ring_1 True = on3 (prop_ring_1 False) bump
prop_ring_1 False = \x y z -> ((x + y) + z) == (x + (y + z))

prop_ring_2 :: Bool -> Zquot -> Zquot -> Bool
prop_ring_2 True = on (prop_ring_2 False) bump
prop_ring_2 False = \x y -> (x + y) == (y + x)

prop_ring_3 :: Bool -> Zquot -> Zquot -> Zquot -> Bool
prop_ring_3 True = on3 (prop_ring_3 False) bump
prop_ring_3 False = \x y z -> ((x * y) * z) == (x * (y * z))

prop_ring_4 :: Bool -> Zquot -> Zquot -> Bool
prop_ring_4 True = on (prop_ring_4 False) bump
prop_ring_4 False = \x y -> (x * y) == (y * x)

prop_ring_5 :: Bool -> Zquot -> Zquot -> Zquot -> Bool
prop_ring_5 True = on3 (prop_ring_5 False) bump
prop_ring_5 False = \x y z -> ((x + y)*z) == (x*z  + y*z)

