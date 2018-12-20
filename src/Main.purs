module Main where

import Boid (Boid, acceleration, alignment, cohesion, randomBoid, randomBoids, separation, visibilitySphere)
import BoidDrawing as Drawing
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Prelude (Unit, bind, const, discard, negate, pure, unit, (<$>))
import Signal (foldp, runSignal) as S
import Signal.DOM (animationFrame) as S
import Vector (Vector, addV, scalarMul)

-- Parameters for the model to be tweaked to our liking
xMin :: Number
xMin = 150.0

xMax :: Number
xMax = 900.0

vMin :: Number
vMin = -0.3

vMax :: Number
vMax = 0.3

dims :: Int
dims = 2

timeStep :: Number
timeStep = 1.0

modifier :: Number
modifier = 3.0

weight_cohesion :: Number
weight_cohesion = 0.02

weight_alignment :: Number
weight_alignment = 0.0005

weight_separation :: Number
weight_separation = 0.001

visibility_radius :: Number
visibility_radius = 150.0

weightConstants :: List Number
weightConstants = weight_cohesion : weight_alignment : weight_separation : Nil
----- Some hardcoded boids -----
-- x :: Vector Number
-- x = V (380.0:300.0:Nil)

-- y :: Vector Number
-- y = V (400.0:260.0:Nil)

-- z :: Vector Number
-- z = V (420.0:280.0:Nil)

-- u :: Vector Number
-- u = V (415.0:255.0:Nil)

-- v :: Vector Number
-- v = V (0.5: 1.0:Nil)

-- boid1 :: Boid
-- boid1 = Tuple x v 

-- boid2 :: Boid
-- boid2 = Tuple y v 

-- boid3 :: Boid
-- boid3 = Tuple z v 

-- boid4 :: Boid
-- boid4 = Tuple u v 

-- listOfBoids :: List Boid
-- listOfBoids = boid1:boid2:boid3:boid4:Nil

-- Application of parameters
cohesion' :: Boid -> List Boid -> Vector Number
cohesion' = cohesion timeStep

alignment' :: List Boid -> Vector Number
alignment' = alignment timeStep

separation' :: Boid -> List Boid -> Vector Number
separation' = separation timeStep

acceleration' :: Vector Number -> Vector Number -> Vector Number -> Vector Number
acceleration' = acceleration weightConstants

boidFactory :: forall a. a -> Effect Boid
boidFactory = const (randomBoid dims xMin xMax vMin vMax)

main :: Effect Unit
main = do
  log "** Boid Simulation **"
  -- ref <- new listOfBoids
  canvas <- getCanvasElementById "canvas"
  boids <- randomBoids boidFactory 60 
  case canvas of 
    Just canvas -> do 
      ctx <- getContext2D canvas
      frames <- S.animationFrame
      let simulation = S.foldp (const iteration) boids frames
      S.runSignal (render ctx <$> simulation)
    Nothing -> pure unit
  log "** End of Simulation **"
  

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
                pos'	    = pos `addV` (scalarMul timeStep vel')
                vel'	    = vel `addV` (scalarMul timeStep acc)

render :: Context2D -> List Boid -> Effect Unit
render context state = do
  Drawing.clearCanvas context
  Drawing.drawBoids context state
  pure unit
