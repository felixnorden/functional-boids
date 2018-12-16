module Vector where

import Data.List
import Data.Show

import Data.Function (($))
import Data.Functor ((<$>))
import Data.List.Lazy (replicate)
import Data.List.NonEmpty (filter)
import Data.Ring ((+), (*), add, mul, negate)
import Data.Tuple.Nested (Tuple3, tuple3, get1)
import Prelude ((<>), (/=), (<<<), (<), (/))

data Vector = V (List Number)


vector :: List Number -> Vector
vector (a:as) = V $ a:as
vector _      = V Nil

zero :: Int -> Vector
zero dim = V (replicate dim 0.0)

instance showVector :: Show Vector where
  show (V nums) = "(" <> components nums <> ")" where
    components (x:Nil)  = show x    
    components (x:xs)   = show x <> ", " <> (components xs)
    components (_)      = ""

addV :: Vector -> Vector -> Vector
addV (V as) (V bs) = V $ zipWith (+) as bs

subV :: Vector -> Vector -> Vector
subV a b = addV a $ scalarMul (-1.0) b
-- mulV :: Vector -> Vector -> Vector
-- mulV (V as) (V bs) = V (zipWith (*) as bs)

scalarMul :: Number -> Vector -> Vector
scalarMul x (V bs) = V ((mul x) <$> bs)

absV :: Vector -> Number
absV (V as) = foldr (\x -> add (x*x)) 0 as 
-- instance semiringVector :: Semiring Vector where
  -- add = addV
  -- mul = mulV

type Acceleration = Vector
type Position     = Vector
type Velocity     = Vector
type Boid         = Tuple3 Position Velocity Acceleration

visibilitySphere :: Boid -> Array Boid -> Number -> List Boid
visibilitySphere boid boids r = filter (\b -> absV(get1 boid `subV` get1 b) < r  ) 
                                <<< filter (\b -> b /= boid) boids

-- visibilitySpheres :: Array Boids -> Number -> Array (List Boid)

cohesion :: Boid -> List Boid -> Position
cohesion boid boids = foldr (addV <<< get1)  boids / (length boids)