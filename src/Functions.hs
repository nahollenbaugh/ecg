module Functions
  ( on
  , on2
  , on3
  , on4
  , on5
  , aon2
  , diag
  , Operation
  , ev
  , operation
  , combineWith
  ) where 
import Zquot

on :: (a -> a -> c) -> (b -> a) -> b -> b -> c
on f u a b = f (u a) (u b)
on2 = on

aon2 f u a b = f <*> (u <*> a) <*> (u <*> b)

on3 :: (a -> a -> a -> c) -> (b -> a) -> b -> b -> b -> c
on3 f u a b c = f (u a) (u b) (u c)
on4 f u a b c d = f (u a) (u b) (u c) (u d)
on5 f u a b c d e= f (u a) (u b) (u c) (u d) (u e)


diag :: (a -> a -> b) -> a -> b
diag f a = f a a

data Operation a =
  Operation (Zquot -> Zquot -> Zquot) (Operation a) (Operation a)
  | Id Zquot
-- data Operation a =
--     Operation (a -> a -> a) (Operation a) (Operation a)
--   | Id a
ev (Id a) = a
ev (Operation f u v) = (on f ev) u v
operation f = on (Operation f) Id


combineWith _ _ [] = []
combineWith _ [] _ = []
combineWith f (x:xs) ys = (map (f x) ys) ++ (combineWith f xs ys)
-- (map ((flip mod 180) . toLift . (diag (*))) $ enumerateZmod $ Value 180 0) == (map ((flip mod 180) . (diag (*))) [1..180])



