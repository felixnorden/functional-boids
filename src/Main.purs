module Main where

import Control.Monad.State
import Control.Monad.State.Class
import Prelude

import Boid (Boid, acceleration, alignment, cohesion, separation, visibilitySphere)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect, whileE)
import Effect.Console (log, logShow)
import Effect.Ref (modify', modify_, new, read, write)
import Prelude (Unit(..), show, ($), (*), (<$>), discard, pure, unit, (+), bind, (<<<))
import Vector (Vector(..), scalarMul)

-- Parameters for the model to be tweaked to our liking
timeStep :: Number
timeStep = 1.0

weight_cohesion :: Number
weight_cohesion = 1.0

weight_alignment :: Number
weight_alignment = 0.5

weight_separation :: Number
weight_separation = 1.3

visibility_radius :: Number
visibility_radius = 10.0

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
boid1 = Tuple x y 

boid2 :: Boid
boid2 = Tuple y z 

boid3 :: Boid
boid3 = Tuple z x 

listOfBoids :: List Boid
listOfBoids = boid1:boid2:boid3:Nil

initialState :: State (List Boid) Unit
initialState = put listOfBoids

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
  log "** Boid Simulation **"
  ref <- new listOfBoids
  whileE (pure true) do
    state <- read ref
    logShow state
    modify_ iteration ref 
    pure unit
  log "** End of Simulation **"
  

forever :: Effect(List Boid) -> Effect(List Boid)
forever boids = do 
  boids' <- boids
  logShow boids'
  forever <<< pure <<< iteration $ boids'
      -- where forever' :: List Boid -> List Boid
      --       forever' Nil	  = Nil
      --       forever' boids' = forever' (pure(iteration boids'))

iteration :: List Boid -> List Boid
iteration boids = iterate <$> boids
  where iterate boid = Tuple pos' vel'
          where c         = cohesion' boid vs
                l         = alignment' vs
                s         = separation' boid vs
                acc       = acceleration' c l s
                vs        = visibilitySphere boid boids visibility_radius
                pos       = fst boid
                vel       = snd boid
                pos'	    = scalarMul (timeStep * timeStep) acc
                vel'	    = scalarMul timeStep acc