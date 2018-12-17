module Vector where

import Data.List
import Data.Show
import Prelude
import Prelude
import Prelude

import Data.Foldable (and)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (length, filter, zipWith)
import Data.Maybe (Maybe(..))
import Data.Ring ((+), (*), add, mul, negate)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (T3, get1)
import Data.Unfoldable (unfoldr)
import Prelude ((<>), (/=), (<<<), (<), (/))

data Vector a = V (List a)


vector :: forall a. Ring a => List a -> Vector a
vector (a:as) = V $ a:as
vector _      = V Nil

zeroV :: Int -> Vector Number
zeroV dim = V $ unfoldr nextZero dim
  where   nextZero :: Int -> Maybe (Tuple Number Int)
          nextZero n | n <= 0     = Nothing
                     | otherwise  = Just (Tuple 0.0 (n-1))

instance showVector :: (Show a) => Show (Vector a) where
  show (V nums) = "(" <> components nums <> ")" where
    components (x:Nil)  = show x    
    components (x:xs)   = show x <> ", " <> (components xs)
    components (_)      = ""

instance eqVector :: Eq a => Eq (Vector a) where
  eq (V xs) (V ys) = and terms
    where terms = zipWith (==) xs ys 

addV :: forall a. Semiring a => Vector a -> Vector a -> Vector a
addV (V as) (V bs) = V $ zipWith (+) as bs

subV :: forall a. Ring a => Vector a -> Vector a -> Vector a
subV a b = addV a $ scalarMul (negate one) b

-- mulV :: Vector -> Vector -> Vector
-- mulV (V as) (V bs) = V (zipWith (*) as bs)

scalarMul :: forall a. Semiring a => a -> Vector a -> Vector a
scalarMul x (V bs) = V ((mul x) <$> bs)

absV :: forall a. Semiring a => Vector a -> a
absV (V xs) = foldr (\x -> add (x*x)) zero xs
-- instance semiringVector :: Semiring Vector where
  -- add = addV
  -- mul = mulV

type Acceleration = Vector Number
type Position     = Vector Number
type Velocity     = Vector Number
type Boid         = T3 Position Velocity Acceleration

visibilitySphere :: Boid -> List Boid -> Number -> List Boid
visibilitySphere boid boids r = filter (\b' -> absV (get1 boid `subV` get1 b') < r  ) $ filter (\b -> b /= boid) boids


cohesion :: List Boid -> Position
cohesion boids = foldr (addV <<< get1) (zeroV 0) boids -- / (length boids)
  where vec  = head boids
        dims = case vec of
            Just boid   -> dimensions (fst boid)
            Nothing     -> 0
        dimensions (V xs) = length xs