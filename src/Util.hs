module Util (bump, nextPrime, equalAsSets, subset, isIn, removeDuplicates, remove) where

import Zquot

bump :: Zquot -> Zquot
bump Undefined = Undefined
bump (Value p a) = Value (p+1) a

nextPrime :: (Integral a) => a -> a
nextPrime n = if isPrime n
              then n
              else nextPrime (n+1)
  where
    fsqrt n = head (filter (\x -> x * x >= n) [1..])
    isPrime :: (Integral a) => a -> Bool
    isPrime 0 = False
    isPrime 1 = False
    isPrime 2 = True
    isPrime n = [] == filter (\x -> (quot n x) * x == n) [2..(fsqrt n)]

equalAsSets :: (Eq a) => [a] -> [a] -> Bool
equalAsSets xs ys = subset xs ys && subset ys xs
subset [] ys = True
subset (x:xs) ys = isIn ys x && subset xs ys
isIn [] _ = False
isIn (y:ys) x = (y == x) || isIn ys x

zipWithPad :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithPad _ _ _ []     []     = []
zipWithPad x _ f []     (y:ys) = (f x y):(map (f x) ys)
zipWithPad _ y f (x:xs) []     = (f x y):(map (flip f y) xs)
zipWithPad u v f (x:xs) (y:ys) = (f x y):(zipWithPad u v f xs ys)

rotate n xs = h n xs []
  where
    h _ []     bs = reverse bs
    h 0 xs     bs = xs ++ (reverse bs)
    h n (x:xs) bs = h (n-1) xs (x:bs)

allPairs _ []     _  = []
allPairs f (x:xs) ys = (map (f x) ys) ++ (allPairs f xs ys)

removeDuplicates [x] = [x]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : (remove x $ removeDuplicates xs)
remove x [] = []
remove x (y:ys)
  | x == y    = remove x ys
  | otherwise = y:(remove x ys)
