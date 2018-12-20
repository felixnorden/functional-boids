module Main where

import Control.Monad.State (State)
import Control.Monad.State.Class (put)
import Prelude (Unit, bind, const, discard, pure, unit, ($), (*), (<$>), (<<<))

import Boid (Boid, acceleration, alignment, cohesion, separation, visibilitySphere)
import BoidDrawing as Drawing
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log, logShow)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Prelude (Unit, bind, const, discard, pure, unit, ($), (*), (<$>), (<<<))
import Signal as S
import Signal.DOM as S
import Vector (Vector(..), scalarMul)

-- Parameters for the model to be tweaked to our liking
timeStep :: Number
timeStep = 0.5

weight_cohesion :: Number
weight_cohesion = 0.15

weight_alignment :: Number
weight_alignment = 0.5

weight_separation :: Number
weight_separation = 0.3

visibility_radius :: Number
visibility_radius = 100.0

weightConstants :: List Number
weightConstants = weight_cohesion : weight_alignment : weight_separation : Nil
----- Some hardcoded boids -----
x :: Vector Number
x = V (380.0:300.0:Nil)

y :: Vector Number
y = V (400.0:260.0:Nil)

z :: Vector Number
z = V (420.0:280.0:Nil)

u :: Vector Number
u = V (415.0:255.0:Nil)

v :: Vector Number
v = V (0.5: 1.0:Nil)

boid1 :: Boid
boid1 = Tuple x v 

boid2 :: Boid
boid2 = Tuple y v 

boid3 :: Boid
boid3 = Tuple z v 

boid4 :: Boid
boid4 = Tuple u v 

listOfBoids :: List Boid
listOfBoids = boid1:boid2:boid3:boid4:Nil

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
  -- ref <- new listOfBoids
  canvas <- getCanvasElementById "canvas"
  case canvas of 
    Just canvas -> do 
      ctx <- getContext2D canvas
      frames <- S.animationFrame
      let simulation = S.foldp (const iteration) listOfBoids frames
      S.runSignal (render ctx <$> simulation)
    Nothing -> pure unit
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

render :: Context2D -> List Boid -> Effect Unit
render context state = do
  Drawing.clearCanvas context
  Drawing.drawBoids context state
  pure unit
