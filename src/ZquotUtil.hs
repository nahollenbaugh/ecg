module ZquotUtil
  ( isSquare
  , valuation
  , sqrtModp
  , intSqrt
  , cornacchia
  ) where

import Zquot


-- ass p prime 
isSquare x@(Value 2 _) = True
isSquare x@(Value p _) = (isZero x) || (pow x (quot (p - 1) 2)) == 1
isSquare Undefined = False
  
cornacchia d p
  = let k = sqrtModp $ -(Value p d)
    in case k of
         Undefined -> Nothing
         _  -> let  xz = (mod (toLift k) (abs p))
                    t  =  if xz > (quot p 2)
                          then p - xz
                          else xz
                    b  = h p t (intSqrt p)
                    c  = quot (p-(b*b)) d
                    y  = intSqrt c
                   in if c*d == p - b*b && y*y == c
                      then Just (b,y)
                      else Nothing
  where
    h a b l = if b > l
              then h b (mod a b) l
              else b

intSqrt :: (Integral a) => a -> a
intSqrt m 
  | m < 0 = - intSqrt (-m)
  | otherwise = intSqrt' m 1 m
-- yes (a+b)/2 is bad. no it is not meaningfully bad.  
  where 
    intSqrt' m a b 
      = let s = quot (a + b) 2
      in if (s*s <= m) && ((s+1)*(s+1) > m)
         then s
         else if s*s < m
              then intSqrt' m s b
              else intSqrt' m a s

-- page 31ff of Cohen 
sqrtModp :: Zquot -> Zquot
sqrtModp Undefined   = Undefined
sqrtModp x@(Value p v)
  | isZero x             = x
  | p == 2               = x
  | (mod (abs p) 4) == 3 = let b = pow x (quot ((abs p) + 1) 4)
                           in if b*b == x
                              then b
                              else Undefined
  | (mod (abs p) 8) == 5 = let s = pow x (quot ((abs p) - 5) 8)
                               b = if s * s * x == 1
                                   then s * x
                                   else (pow 2 (quot ((abs p) - 1) 4)) * s * x
                           in if b*b == x
                              then b
                              else Undefined
  | otherwise            = tonelliShanks x

tonelliShanks a@(Value p _) =
  let
    (e,twos)  = valuation ((abs p) - 1) 2
    q         = quot ((abs p) - 1) twos
    z         = pow (head (filter (\x -> not (isSquare x)) (enumerateZquot a))) q
    xx        = pow a (quot (q-1) 2)
  in h (a*xx*xx) (a*xx) z e
  where
    h :: Zquot -> Zquot -> Zquot -> Integer -> Zquot
    h b x y r = if b == 1
                then x
                else let (_,m) = (head (filter (\(y,_) -> y == 1)
                                        (iterate (\(s, m) -> (s*s,m+1))
                                          (b,0))))
                     in if r == m
                        then Undefined
                        else let t = pow y (pow 2 ((r - m) - 1))
                             in h (t*t*b) (x*t) (t*t) m

valuation :: Integer -> Integer -> (Integer,Integer)
valuation x p
  | mod x p == 0 = let (e,q) = valuation (quot x p) p
                   in (1+e,q*p)
  | otherwise    = (0,1)
