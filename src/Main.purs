module Main where

import Boid (Boid, acceleration, alignment, cohesion, separation)
import Data.List (List(..), (:))
import Data.Tuple.Nested (tuple3)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit)
import Vector (Vector(..))

-- Parameters for the model to be tweaked to our liking
timeStep :: Number
timeStep = 1.0

weight_cohesion :: Number
weight_cohesion = 1.0

weight_alignment :: Number
weight_alignment = 1.0

weight_separation :: Number
weight_separation = 1.0

weightConstants :: List Number
weightConstants = weight_cohesion : weight_alignment : weight_separation : Nil
----- Some hardcoded boids -----
x :: Vector Number
x = V (1.0:2.0:Nil)

y :: Vector Number
y = V (3.0:4.0:Nil)

z :: Vector Number
z = V (5.0:6.0:Nil)

boid1 :: Boid
boid1 = tuple3 x y z

boid2 :: Boid
boid2 = tuple3 y z x

boid3 :: Boid
boid3 = tuple3 z x y

listOfBoids :: List Boid
listOfBoids = boid1:boid2:boid3:Nil

-- Application of parameters
cohesion' :: Boid -> List Boid -> Vector Number
cohesion' = cohesion timeStep

alignment' :: List Boid -> Vector Number
alignment' = alignment timeStep

separation' :: Boid -> List Boid -> Vector Number
separation' = separation timeStep

acceleration' :: Vector Number -> Vector Number -> Vector Number -> Vector Number
acceleration' = acceleration weightConstants

main :: Effect Unit
main = do
  log "Hello world"
