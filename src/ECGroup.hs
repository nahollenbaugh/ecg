{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- https://wiki.haskell.org/The_Monad.Reader/Issue5/Number_Param_Types

module ECGroup
  ( Point(..)
  , Curve(..)
  , contains
  , curve
  , point
  , en
  , ep
  , em
  , edouble
  , ezact
  , isUndefinedPoint 
  ) where


import Zquot
import ZquotUtil
import Test.Framework
import Control.Monad 
import Functions

-- (notation from Silverman 42ff)

data Point = Zero | A2 Zquot Zquot deriving (Eq)

-- (not necessarily prime characteristic, a_1, a_2, a_3, a_4, a_6)
data Curve = Curve Zquot Zquot Zquot Zquot Zquot deriving (Eq)

-- curve :: (Integral a, Integral b) => b -> a -> a -> a -> a -> a -> Curve
curve n a b c d f = on5 Curve (Value n) a b c d f
-- point :: (Integral a, Integral b) => b -> a -> a -> Curve
point n x y = on2 A2 (Value n) x y 

contains :: Curve -> Point -> Bool
contains _ Zero = True
contains (Curve a b c d f) (A2 x y) = y*y + a*x*y + c*y == x*x*x + b*x*x + d*x + f

en :: Curve -> Point -> Point
en _ Zero          = Zero
en (Curve a b c d f) (A2 x y) = A2 x (-y-a*x-c)

ep :: Curve -> Point -> Point -> Point
ep _ p Zero                                = p
ep _ Zero p                                = p
ep (Curve a b c d f) (A2 x y) (A2 u v)
  | (x == u) && (0 == y + v + a*u + c) = Zero
  | otherwise = let la = if x == u
                         then (3*x*x + 2*b*x + d - a*y) / (2*y + a*x + c)
                         else (v - y) / (u - x)
                    nu = if (x == u)
                         then (-x*x*x + d*x + 2*f - c*y) / (2*y + a*x + c)
                         else (y*u - v*x) / (u - x)
                    s  = la*la + a*la - b - x - u
                in A2 s (-(la+a)*s - nu - c)

em :: Curve -> Point -> Point -> Point
em c p q = ep c p (en c q)

edouble :: Curve -> Point -> Point
edouble c p = ep c p p

ezact :: (Integral b) => Curve -> b -> Point -> Point
ezact c n p = if (n < 0)
              then en c (ezactp c (-n) p)
              else ezactp c n p
  where
    ezactp :: (Integral b)
           => Curve -> b -> Point -> Point
    ezactp _ 0 _ = Zero 
    ezactp c n p = let s = if (mod n 2) == 0
                         then Zero
                         else p
                       b = ezactp c (quot n 2) (edouble c p)
                   in ep c s b 


isUndefinedPoint :: Point -> Bool
isUndefinedPoint Zero          = False
isUndefinedPoint (A2 x y) = isUndefined x || isUndefined y

instance Show Point where
  show Zero     = "o"
  show (A2 x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

instance Show Curve where
  show (Curve a b c d f)
    = "{y^2"
      ++ (if isZero a
          then ""
          else " + "
           ++ (if isOne a 
               then ""
               else show a)
           ++ "xy")
      ++ (if isZero c
          then ""
          else " + "
           ++ (if isOne c
               then ""
               else show c)
           ++ "y")
      ++ " = x^3"
      ++ (if isZero b
          then ""
          else " + "
           ++ (if isOne b
               then ""
               else show b)
           ++ "x^2")
      ++ (if isZero d
          then ""
          else " + "
           ++ (if isOne d
               then ""
               else show d)
           ++ "x")
      ++ (if isZero f
          then ""
          else " + " ++ show f)
      ++ "}"
      ++ " / " ++ let (Value m _) = f
                  in if m /= 0
                     then "F_"++show m
                     else "Z"


instance Arbitrary Curve where
  arbitrary = curve <$> (liftM (nextPrime . (+2) . abs) arbitrary)
              <*> arbitrary 
              <*> arbitrary 
              <*> arbitrary 
              <*> arbitrary 
              <*> arbitrary 
    where
      nextPrime n = head $ filter isPrime [n..]
      isPrime n = null $ filter (\a -> mod n a == 0) [2..intSqrt n]
instance Arbitrary Point where
  arbitrary = (liftM2 A2) arbitrary arbitrary 

-- $j$-invariant $j=c_4^3/\De$




