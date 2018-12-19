module Boid where

import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (List(..), filter, zipWith, (:))
import Data.Ring ((*))
import Data.Tuple (Tuple, fst, snd)
import Prelude ((/), (<), (<<<))
import Vector (Vector, absV, averageV, scalarMul, subV, sumV)


-- type Acceleration = Vector Number
type Position     = Vector Number
type Velocity     = Vector Number
type Boid         = Tuple Position Velocity-- Acceleration


visibilitySphere :: Boid -> List Boid -> Number -> List Boid
visibilitySphere boid boids r = filter (\b -> absV (fst boid `subV` fst b) < r) <<< filter (\b -> b /= boid) $ boids


-- Takes a visibility sphere and calculates the mass centrum of the sphere
massCentrum :: List Boid -> Vector Number
massCentrum boids = averageV $ fst <$> boids


-- Returns the cohesion component of the acceleration vector
cohesion :: Number -> Boid -> List Boid -> Vector Number
cohesion timeStep boid boids = scalarMul scalar distance
  where scalar = 1.0 / (timeStep * timeStep)
        distance = subV (massCentrum boids) (fst boid)

-- Returns the alignment component of the acceleration vector
alignment :: Number -> List Boid -> Vector Number
alignment timeStep boids = scalarMul scalar averageVelocity
  where scalar = 1.0 / timeStep
        averageVelocity = averageV $ snd <$> boids

-- Returns the separation component of the acceleration vector
separation :: Number -> Boid -> List Boid -> Vector Number
separation timeStep boid boids = scalarMul scalar totalDistance
  where scalar = 1.0 / (timeStep * timeStep)
        totalDistance = sumV $ subV boidPos <$> boidsPos
        boidPos = fst boid
        boidsPos = fst <$> boids

-- Returns the resulting acceleration vector for a boid 
-- (c = cohesion, l = alignment, s = separation)
acceleration :: List Number -> Vector Number -> Vector Number -> Vector Number -> Vector Number
acceleration constants c l s = sumV $ zipWith scalarMul constants (c:l:s:Nil)