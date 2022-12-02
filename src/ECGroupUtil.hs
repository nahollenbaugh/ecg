module ECGroupUtil ( listPoints
                   , singular
                   , disc
                   , b2
                   , b4
                   , b6
                   , b8
                   , c4
                   , c6
                   , enumerate
                   , base
                   , getNonzero
                   , countPointsEnumerate
                   ) where 

import ECGroup
import Zquot
import ZquotUtil
import Functions

listPoints curve = foldl (flip $ (++) . show) "" $ enumerate curve

-- probably not great? Enumerates $\Z/n^2$ and gives the ones that live on
-- the curve.  Pretty sure enumerate $\Z/n$ and compute square roots is
-- faster. Why'd I do this? 
enumerate :: Curve -> [Point]
enumerate c@(Curve (Value n _) _ _ _ _)
  = Zero:(filter (\x -> contains c x)
          [A2 (Value n x) (Value n y) | x <- [0..(n-1)], y <- [0..(n-1)]])

-- Imposes some order on $(\Z/n)^2$ and interates through it until getting
-- to a pont on the curve, for the purposes of sort-of-randomly generating
-- points on curves for small $n$.
getNonzero :: Curve -> Point -> Point
getNonzero _ Zero = Zero
getNonzero c p = getNonzero' c p 0
  where
    getNonzero' c p@(A2 (Value n x) (Value _ y)) m =
      if contains c p
      then p
      else if m >= n*n
           then A2 Undefined Undefined
           else if (mod m n == 0)
                then getNonzero' c (A2 (Value n (x+1)) (Value n y)) (m+1)
                else getNonzero' c (A2 (Value n x) (Value n (y+1))) (m+1)

-- rmk there is a typo silverman here 
-- b2 (Curve a b c d f) = a*a + 4*d
b2 (Curve a b c d f) = a*a + 4*b
b4 (Curve a b c d f) = 2*d + a*c
b6 (Curve a b c d f) = c*c + 4*f
b8 (Curve a b c d f) = a*a*f + 4*b*f - a*c*d + b*c*c - d*d
disc c = -(b2 c)*(b2 c)*(b8 c) - 8*(b4 c)*(b4 c)*(b4 c)
  - 27*(b6 c)*(b6 c) + 9*(b2 c)*(b4 c)*(b6 c)

c4 c = (b2 c)*(b2 c) - 24*(b4 c)
c6 c = -(b2 c)*(b2 c)*(b2 c) + 36*(b2 c)*(b4 c) - 216*(b6 c)

singular :: Curve -> Bool
singular c = disc c == 0


base (Curve (Value m _) _ _ _ _) = m

countPointsEnumerate = toInteger . length . enumerate
