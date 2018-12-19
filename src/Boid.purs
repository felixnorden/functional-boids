module Boid where

import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (List(..), filter, zipWith, (:))
import Data.Ring ((*))
import Data.Tuple.Nested (Tuple3, get1, get2)
import Prelude ((/), (<), (<<<))
import Vector (Vector, absV, averageV, scalarMul, subV, sumV)


type Acceleration = Vector Number
type Position     = Vector Number
type Velocity     = Vector Number
type Boid         = Tuple3 Position Velocity Acceleration


visibilitySphere :: Boid -> List Boid -> Number -> List Boid
visibilitySphere boid boids r = filter (\b -> absV (get1 boid `subV` get1 b) < r) <<< filter (\b -> b /= boid) $ boids


-- Takes a visibility sphere and calculates the mass centrum of the sphere
massCentrum :: List Boid -> Vector Number
massCentrum boids = averageV $ get1 <$> boids


-- Returns the cohesion component of the acceleration vector
cohesion :: Number -> Boid -> List Boid -> Vector Number
cohesion timeStep boid boids = scalarMul scalar distance
  where scalar = 1.0 / (timeStep * timeStep)
        distance = subV (massCentrum boids) (get1 boid)

-- Returns the alignment component of the acceleration vector
alignment :: Number -> List Boid -> Vector Number
alignment timeStep boids = scalarMul scalar averageVelocity
  where scalar = 1.0 / timeStep
        averageVelocity = averageV $ get2 <$> boids

-- Returns the separation component of the acceleration vector
separation :: Number -> Boid -> List Boid -> Vector Number
separation timeStep boid boids = scalarMul scalar totalDistance
  where scalar = 1.0 / (timeStep * timeStep)
        totalDistance = sumV $ subV boidPos <$> boidsPos
        boidPos = get1 boid
        boidsPos = get1 <$> boids

-- Returns the resulting acceleration vector for a boid 
-- (c = cohesion, l = alignment, s = separation)
acceleration :: List Number -> Vector Number -> Vector Number -> Vector Number -> Vector Number
acceleration constants c l s = sumV $ zipWith scalarMul constants (c:l:s:Nil)