module Main where

import Web.DOM

import Boid (Boid, acceleration, alignment, cohesion, randomBoid, randomBoids, separation, visibilitySphere)
import BoidDrawing as Drawing
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log, logShow)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Prelude (Unit, bind, const, discard, negate, pure, unit, ($), (<$>), (<<<), (=<<))
import Signal (foldp, runSignal) as S
import Signal.DOM (animationFrame) as S
import Vector (Vector, addV, scalarMul)
import Web.DOM.Node (parentElement)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (currentTarget)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (change)
import Web.HTML.HTMLAreaElement (target)
import Web.HTML.HTMLButtonElement as Buttons
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLInputElement as Inputs
import Web.HTML.Window (document)

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

slideEvent :: Effect EventListener
slideEvent = eventListener \slide -> log "slide"

main :: Effect Unit
main = do
  log "** Boid Simulation **"
  browser <- window
  doc <- document browser 

  let parentElement = toNonElementParentNode doc

  canvas <- getCanvasElementById "canvas"
  slider <- getElementById "slider" parentElement
  resetBtn <- getElementById "resetBtn" parentElement

  let resetBtn' = Buttons.fromElement =<< resetBtn
  let slider' = Inputs.fromElement =<< slider
  
  case slider' of
    Just slider' -> do
      handler <- slideEvent
      addEventListener change handler false (Inputs.toEventTarget slider')
    Nothing -> pure unit    
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
