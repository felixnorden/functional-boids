module Vector where

import Data.List
import Data.Show
import Prelude

import Data.Int (toNumber)
import Data.Foldable (and)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (length, filter, zipWith)
import Data.Maybe (Maybe(..))
import Data.Ring ((+), (*), add, mul, negate)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (T3, get1, get2)
import Data.Unfoldable (unfoldr)
import Prelude ((<>), (/=), (<<<), (<), (/))

data Vector a = V (List a)

time :: Number
time = 1.0
weight_cohesion :: Number
weight_cohesion = 1.0
weight_alignment :: Number
weight_alignment = 1.0
weight_separation :: Number
weight_separation = 1.0

----- Some hardcoded boids -----
x = V (1.0:2.0:Nil)
y = V (3.0:4.0:Nil)
z = V (5.0:6.0:Nil)

boid1 = Tuple x (Tuple y z)
boid2 = Tuple y (Tuple z x)
boid3 = Tuple z (Tuple x y)

listOfBoids = boid1:boid2:boid3:Nil


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

averageVector :: List (Vector Number) -> Vector Number
averageVector vecs = scalarMul scalar totalSum
  where scalar = 1.0 / (toNumber $ length vecs)
        totalSum = sumVectors vecs

sumVectors :: List (Vector Number) -> Vector Number
sumVectors vecs = foldr addV (zeroV dims) vecs
  where vec  = head vecs
        dims = case vec of
            Just value   -> dimensions value
            Nothing     -> 0
        dimensions (V xs) = length xs

-- Takes a visibility sphere and calculates the mass centrum of the sphere
massCentrum :: List Boid -> Vector Number
massCentrum boids = averageVector $ get1 <$> boids


-- Returns the cohesion component of the acceleration vector
cohesion :: Boid -> List Boid -> Vector Number
cohesion boid boids = scalarMul scalar distance
  where scalar = 1.0 / (time * time)
        distance = subV (massCentrum boids) (get1 boid)

-- Returns the alignment component of the acceleration vector
alignment :: List Boid -> Vector Number
alignment boids = scalarMul scalar averageVelocity
  where scalar = 1.0 / time
        averageVelocity = averageVector $ get2 <$> boids

-- Returns the separation component of the acceleration vector
separation :: Boid -> List Boid -> Vector Number
separation boid boids = scalarMul scalar totalDistance
  where scalar = 1.0 / (time*time)
        totalDistance = sumVectors $ subV boidPos <$> boidsPos
        boidPos = get1 boid
        boidsPos = get1 <$> boids

