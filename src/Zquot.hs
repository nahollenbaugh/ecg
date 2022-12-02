
module Zquot
  ( Zquot(..)
  , isZero
  , isOne
  , isUndefined
  , enumerateZquot
  , toLift
  , pow
  ) where 

--------------------------------------------------------------------
-- Zquot to represent the finite quotients of the integers, including
-- an Undefined element so as to represent division.
--------------------------------------------------------------------

import Data.Ratio (numerator,denominator)
import Test.Framework
import Control.Monad (liftM2)

data Zquot = Value Integer Integer | Undefined

instance Show Zquot where
  show Undefined   = "Undefined"
  show (Value 0 x) = show x
  show (Value n x) = show (mod x n)

instance Eq Zquot where
  (Value 0 x) == (Value 0 y) = x == y
  (Value 0 x) == (Value n y) = (mod x n) == (mod y n)
  (Value n x) == (Value 0 y) = (mod x n) == (mod y n)
  (Value n x) == (Value m y) = ((n == m) || (n == (-m)))
                               && (rem (x - y) n == 0)
  Undefined == _             = False
  _ == Undefined             = False 

instance Num Zquot where
  Undefined + _             = Undefined
  _ + Undefined             = Undefined
  (Value n x) + (Value m y)
    | (n == 0) && (m /= 0)    = Value m (x + y)
    | (n /= 0) && (m == 0)    = Value n (x + y)
    | (n /= m) && (n /= (-m)) = Undefined
    | otherwise               = Value n (x + y)

  Undefined * _             = Undefined
  _ * Undefined             = Undefined
  (Value n x) * (Value m y)
    | (n == 0) && (m /= 0)    = Value m (x * y)
    | (n /= 0) && (m == 0)    = Value n (x * y)
    | (n /= m) && (n /= (-m)) = Undefined
    | otherwise               = Value n (x * y)

  abs v = v
  signum _ = 1

  fromInteger x = Value 0 x

  negate Undefined   = Undefined
  negate (Value n x) = Value n (-x)

instance Fractional Zquot where 

  fromRational x = if denominator x == 1
                   then Value 0 (numerator x)
                   else Undefined

  recip Undefined      = Undefined
  recip (Value 0 1)    = (Value 0 1)
  recip (Value 0 (-1)) = (Value 0 (-1))
  recip (Value 0 n)    = Undefined
  recip (Value n a)    = if gcd a n == 1
                         then let (_, b) = d n (mod a n)
                              in Value n b
                         else Undefined
    where
      d 1 0 = (1,0)
      d a b = let r = div a b
                  q = mod a b
                  (u,v) = d b q
              in (v,u - (v * r))



-- pow (Value n _) 0 = Value n 1
pow _ 0 = 1
pow x 1 = x
pow x n = if (mod n 2) == 0
          then half * half
          else half * half * x
  where
    half = pow x (quot n 2)

isZero Undefined   = False
isZero (Value 0 x) = 0 == x
isZero (Value n x) = 0 == mod x n

isOne Undefined   = False
isOne (Value 0 x) = 1 == x
isOne (Value n x) = 1 == (mod x n)

isUndefined Undefined = True
isUndefined _         = False

enumerateZquot :: Zquot -> [Zquot]
enumerateZquot (Value 0 _) = []
enumerateZquot x           = d (x + 1) x
  where
    d y x
      | y == x    = [x]
      | otherwise = y:(d (y + 1) x)


-- toLift Undefined = 117
toLift (Value m x) = mod x m


-- rmk: 179 prime, 180 = 2^2*3^2*5
instance Arbitrary Zquot where
  arbitrary = Value 179 <$> arbitrary 
  -- arbitrary = (liftM2 Value) arbitrary arbitrary
  
