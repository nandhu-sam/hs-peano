
module Peano (Peano (Zero)) where

import GHC.Real ((%))

data Peano = Zero 
           | Succ Peano 
           | Neg Peano deriving (Show)


-- nonneg := Zero
--         | Succ <nonneg>
-- pos := Succ <nonneg>
-- neg := Neg <pos>
-- peano := nonneg | neg


instance Enum Peano where
  succ (Neg (Succ Zero)) = Zero
  succ (Neg (Succ x)) = Neg x
  succ x = Succ x

  pred Zero     = Neg (Succ Zero)
  pred (Neg x)  = Neg (Succ x)
  pred (Succ x) = x
  
  toEnum n
    | n == 0    = Zero
    | n < 0     = (Neg . toEnum . negate) n
    | otherwise = (Succ . toEnum . pred) n

  fromEnum Zero = 0
  fromEnum (Succ x) = (succ . fromEnum) x
  fromEnum (Neg x) = (negate . fromEnum) x

instance Eq Peano where
  Zero == Zero = True
  Zero == x = False
  (Succ x) == (Succ y) = x == y
  (Neg x)  == (Neg y)  = x == y
  (Succ _) == (Neg _)  = False
  x == y = y == x

instance Ord Peano where
  Zero <= Zero     = True 
  Zero <= (Succ x) = True
  Zero <= (Neg x)  = False
  (Succ x) <= Zero     = False
  (Succ x) <= (Succ y) = x <= y
  (Succ x) <= _        = False
  (Neg x) <= (Neg y)   = y <= x
  (Neg x) <= _ = True


instance Num Peano where
  negate Zero = Zero
  negate (Neg x) = x
  negate x = Neg x

  abs Zero = Zero
  abs (Neg x) = x
  abs x = x

  signum Zero = Zero
  signum (Succ _) = Succ Zero
  signum (Neg _)  = (Neg (Succ Zero))

  fromInteger n
    | n == 0    = Zero
    | n < 0     = (Neg . fromInteger . negate) n
    | otherwise = (Succ . fromInteger . pred) n

  Zero + x = x
  (Succ x) + (Succ y)       = Succ (Succ (x+y))
  (Succ x) + (Neg (Succ y)) = x + (negate y)
  (Neg x)  + (Neg y)        = Neg (x+y)
  x + y = y + x

  Zero * _ = Zero
  a@(Succ x) * (Neg y) = negate (a*y)
  (Succ x) * y = (x*y) + y
  x * y = y * x
  

instance Real Peano where
  toRational Zero = 0 % 1
  toRational (Neg x) = (negate . toRational) x
  toRational (Succ x) = (succ . toRational) x


instance Integral Peano where
  divMod _ Zero = error "division by zero"
  divMod Zero _ = (Zero, Zero)
  divMod x y = if x>=y
               then let (d, m) = (x-y) `divMod` y in (succ d, m) 
               else (Zero, x)
  

  quotRem _ Zero = error "division by zero"
  quotRem Zero _ = (Zero, Zero)
  quotRem n@(Succ x) d@(Succ y) = divMod n d
  quotRem (Neg x) d@(Succ y) = let (x', y') = quotRem x d
                               in (negate x, negate y)
  quotRem n@(Succ x) (Neg y) = let (x', y') = quotRem n y
                               in (negate x', y)
  quotRem (Neg n) (Neg d) = let (x', y') = quotRem n d
                            in (x', negate y')

  toInteger Zero = 0
  toInteger (Neg x)  = (negate . toInteger) x
  toInteger (Succ x) = (succ . toInteger) x


