{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestECGroup (htf_thisModulesTests) where

import Test.Framework
import Test.Framework.TestInterface
import ECGroup
import ECGroupUtil
import Zquot
import Functions
import Util

test_contains = do
  assertBool $       contains (curve 7 0 0 0 1 1) (point 7 2 5)
  assertBool $ not $ contains (curve 7 0 0 0 1 1) (point 7 2 4)
  assertBool $ not $ contains (curve 7 0 0 0 1 1) (point 3 2 5)
  assertBool $       contains (curve 3 0 0 0 0 1) (point 3 2 0)
  assertBool $       contains (curve 3 0 0 0 1 2) Zero
  assertBool $       contains (curve 4 0 0 0 2 2) (point 4 1 1)
  assertBool $ not $ contains (curve 4 0 0 0 2 2) (point 4 1 2)
  assertBool $       contains (curve 4 0 0 0 2 2) Zero
  assertListsEqualAsSets
    (filter (contains $ curve 7 2 0 1 0 2) [on2 A2 (Value 7) x y | x <- [1..7], y <- [1..7]])
    [point 7 0 1, point 7 0 5, point 7 1 2, point 7 2 3, point 7 2 6, point 7 3 1, point 7 3 6,
     point 7 4 1, point 7 4 4]

test_en_1 = do
  assertEqual (en (curve 5 0 0 0 2 1) (point 5 3 2)) (point 5 3 3)
  assertEqual (en (curve 5 0 0 0 2 1) Zero)          Zero

  assertEqual (en (curve 11 1 3 2 5 7) (point 11 7 5))  (point 11 7 8)
  assertEqual (en (curve 11 1 3 2 5 7) (point 11 9 1))  (point 11 9 10)
  assertEqual (en (curve 11 1 3 2 5 7) (point 11 9 10)) (point 11 9 1)
  assertEqual (en (curve 11 1 3 2 5 7) (point 11 6 1))  (point 11 6 2)
  assertEqual (en (curve 11 1 3 2 5 7) (point 11 6 2))  (point 11 6 1)
  assertEqual (en (curve 11 1 3 2 5 7) (point 11 5 6))  (point 11 5 9)
  assertEqual (en (curve 11 1 3 2 5 7) (point 11 5 9))  (point 11 5 6)
  assertEqual (en (curve 11 1 3 2 5 7) (point 11 4 1))  (point 11 4 4)
  assertEqual (en (curve 11 1 3 2 5 7) (point 11 4 4))  (point 11 4 1)

  assertEqual (en (curve 7 2 0 1 0 2) (point 7 0 1)) (point 7 0 5)
  assertEqual (en (curve 7 2 0 1 0 2) (point 7 0 5)) (point 7 0 1)
  assertEqual (en (curve 7 2 0 1 0 2) (point 7 1 2)) (point 7 1 2)
  assertEqual (en (curve 7 2 0 1 0 2) (point 7 2 3)) (point 7 2 6)
  assertEqual (en (curve 7 2 0 1 0 2) (point 7 2 6)) (point 7 2 3)
  assertEqual (en (curve 7 2 0 1 0 2) (point 7 3 1)) (point 7 3 6)
  assertEqual (en (curve 7 2 0 1 0 2) (point 7 3 6)) (point 7 3 1)
  assertEqual (en (curve 7 2 0 1 0 2) (point 7 4 1)) (point 7 4 4)
  assertEqual (en (curve 7 2 0 1 0 2) (point 7 4 4)) (point 7 4 1)

prop_en_2 c@(Curve a@(Value m _) _ _ _ _) u v
  = if singular c 
    then True
    else let p@(A2 x y) = getNonzero c (A2 (Value m u) (Value m v))
         in if isUndefined x
            -- how many curves of testing size have no points? i.e. is it okay to
            -- throw away these tests
               -- we've (accidentally :] ) run a bunch of tests that failed when
               -- we came across a curve with no points, and they didn't always
               -- fail, so probably not much
            then True
            else (ep c p $ en c p) == Zero

test_ep_1 = do
  assertEqual (ep (curve 5 0 0 0 2 1) (point 5 3 2) Zero)          (point 5 3 2)
  assertEqual (ep (curve 5 0 0 0 2 1) Zero          (point 5 3 2)) (point 5 3 2)
  assertEqual (ep (curve 5 0 0 0 2 1) Zero          Zero)          Zero 
  assertEqual (ep (curve 5 0 0 0 2 1) (point 5 3 2) (point 5 3 2)) (point 5 0 1)

  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5) (point 11 7 5))  (point 11 6 1)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5) (point 11 9 1))  (point 11 5 6)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5) (point 11 9 10)) (point 11 9 1)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5) (point 11 6 1))  (point 11 4 1)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5) (point 11 6 2))  (point 11 7 8)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5) (point 11 5 6))  (point 11 4 4)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5) (point 11 5 9))  (point 11 9 10)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5) (point 11 4 1))  (point 11 5 9)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5) (point 11 4 4))  (point 11 6 2)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5) (point 11 7 5))  (point 11 6 1)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 9 1) (point 11 7 5))  (point 11 5 6)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 9 10) (point 11 7 5)) (point 11 9 1)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 6 1) (point 11 7 5))  (point 11 4 1)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 6 2) (point 11 7 5))  (point 11 7 8)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 5 6) (point 11 7 5))  (point 11 4 4)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 5 9) (point 11 7 5))  (point 11 9 10)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 4 1) (point 11 7 5))  (point 11 5 9)
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 4 4) (point 11 7 5))  (point 11 6 2)

  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 1) (point 7 0 1)) (point 7 3 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 1) (point 7 1 2)) (point 7 2 6)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 1) (point 7 2 3)) (point 7 1 2)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 1) (point 7 2 6)) (point 7 4 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 1) (point 7 3 1)) (point 7 4 4)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 1) (point 7 3 6)) (point 7 0 5)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 1) (point 7 4 1)) (point 7 3 6)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 1) (point 7 4 4)) (point 7 2 3)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 5) (point 7 0 5)) (point 7 3 6)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 5) (point 7 1 2)) (point 7 2 3)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 5) (point 7 2 3)) (point 7 4 4)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 5) (point 7 2 6)) (point 7 1 2)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 5) (point 7 3 1)) (point 7 0 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 5) (point 7 3 6)) (point 7 4 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 5) (point 7 4 1)) (point 7 2 6)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 5) (point 7 4 4)) (point 7 3 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 1 2) (point 7 2 3)) (point 7 0 5)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 1 2) (point 7 2 6)) (point 7 0 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 1 2) (point 7 3 1)) (point 7 4 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 1 2) (point 7 3 6)) (point 7 4 4)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 1 2) (point 7 4 1)) (point 7 3 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 1 2) (point 7 4 4)) (point 7 3 6)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 3) (point 7 2 3)) (point 7 3 6)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 3) (point 7 3 1)) (point 7 2 6)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 3) (point 7 3 6)) (point 7 3 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 3) (point 7 4 1)) (point 7 0 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 3) (point 7 4 4)) (point 7 4 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 6) (point 7 2 6)) (point 7 3 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 6) (point 7 3 1)) (point 7 3 6)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 6) (point 7 3 6)) (point 7 2 3)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 6) (point 7 4 1)) (point 7 4 4)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 6) (point 7 4 4)) (point 7 0 5)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 3 1) (point 7 3 1)) (point 7 2 3)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 3 1) (point 7 4 1)) (point 7 0 5)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 3 1) (point 7 4 4)) (point 7 1 2)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 3 6) (point 7 3 6)) (point 7 2 6)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 3 6) (point 7 4 1)) (point 7 1 2)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 3 6) (point 7 4 4)) (point 7 0 1)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 4 1) (point 7 4 1)) (point 7 2 3)
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 4 4) (point 7 4 4)) (point 7 2 6)

  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 0 1) (point 7 0 5)) Zero
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 1 2) (point 7 1 2)) Zero
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 2 3) (point 7 2 6)) Zero
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 3 1) (point 7 3 6)) Zero
  assertEqual (ep (curve 7 2 0 1 0 2) (point 7 4 1) (point 7 4 4)) Zero

  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 7 5)  (point 11 7 8))  Zero
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 9 1)  (point 11 9 10)) Zero
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 9 10) (point 11 9 1))  Zero
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 6 1)  (point 11 6 2))  Zero
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 6 2)  (point 11 6 1))  Zero
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 5 6)  (point 11 5 9))  Zero
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 5 9)  (point 11 5 6))  Zero
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 4 1)  (point 11 4 4))  Zero
  assertEqual (ep (curve 11 1 3 2 5 7) (point 11 4 4)  (point 11 4 1))  Zero


prop_ep_2 curve@(Curve a@(Value m _) b c d f) u v u' v' = 
  let p@(A2 x y)    = getNonzero curve $ point m u v
      p'@(A2 x' y') = getNonzero curve $ point m u' v'
  in
    if isUndefined x
       || (disc curve) == 0
    then True
    else let sum = ep curve p p'
         in if sum == Zero
            then p' == en curve p
            else let (A2 u v) = sum
                 in (contains curve sum)
                    && let dn = if x == x' then 2*y + a*x + c            else x' - x
                           la = if x == x' then 3*x*x + 2*b*x + d - a*y  else y' - y
                           nu = if x == x' then -x*x*x + d*x + 2*f - c*y else y*x' - y'*x
                       in dn*dn*u == la*la + a*la*dn - (b+x+x')*dn*dn
                          && dn*v == -(la+a*dn)*u - (nu+c*dn)

prop_ezact c@(Curve a@(Value m _) _ _ _ _) n p' =
  if singular c
  then True
  else let p   = getNonzero c p'
           sum = foldl (\x y -> ep c x y) Zero
                 (take (abs (quot n 7)) (repeat p))
       in if isUndefinedPoint sum
          then True
          else if n > 0
               then sum == ezact c (quot n 7) p
               else en c sum == ezact c (quot n 7) p


prop_group_1 c@(Curve (Value m _) _ _ _ _) u v u' v'
  = if singular c
    then True
    else let x = getNonzero c $ point m u v
             y = getNonzero c $ point m u' v'
         in case x of
              A2 Undefined _ -> True
              _ ->  (ep c x y) == (ep c y x)
prop_group_2 c@(Curve (Value m _) _ _ _ _) u v
  = if singular c
    then True
    else let x = getNonzero c $ point m u v
         in case x of
              A2 Undefined _ -> True
              _ -> ep c x Zero == x && ep c Zero x == x
prop_group_3 c@(Curve (Value m _) _ _ _ _) u v u' v' u'' v''
  = if singular c
    then True
    else let x = getNonzero c $ point m u v
             y = getNonzero c $ point m u' v'
             z = getNonzero c $ point m u'' v''
         in case x of
              A2 Undefined _ -> True
              _ -> ep c (ep c x y) z == ep c x (ep c y z)

