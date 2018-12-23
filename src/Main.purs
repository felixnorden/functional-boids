module Main where


import Boid (Boid, acceleration, alignment, cohesion, randomBoid, randomBoids, separation, visibilitySphere)
import BoidDrawing as Drawing
import Data.Int (fromString)
import Data.List (List(..), (:), length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D)
import Prelude (Unit, bind, const, discard, negate, pure, unit, ($), (<$>), (=<<))
import Signal (get)
import Signal.Channel (Channel, channel, send, subscribe)
import Vector (Vector, addV, scalarMul)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event as E
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (change, click)
import Web.HTML.HTMLButtonElement as Buttons
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLInputElement as Inputs
import Web.HTML.Window (document, requestAnimationFrame)

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

slideEvent :: Channel (List Boid) ->  Effect EventListener
slideEvent ch = eventListener \slide -> do
  slider <- pure $ Inputs.fromEventTarget =<< E.currentTarget slide
  case slider of
    Just slider -> do slideValue <- Inputs.value slider
                      log slideValue
                      intVal <- pure $ fromString slideValue
                      case intVal of
                        Just val -> do boids <- randomBoids boidFactory val
                                       send ch boids
                        Nothing -> pure unit                                   
    Nothing -> pure unit

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

  boids <- randomBoids boidFactory 50 
  boidChannel <- channel (boids)
  
  let subscription = subscribe boidChannel
  case resetBtn' of
    Just btn -> do 
      handler <- eventListener \click -> do
                  btn <- pure $ Buttons.fromEventTarget =<< E.currentTarget click
                  case btn of
                    Just btn' -> do 
                      currentBoids <- get subscription
                      newBoids <- randomBoids boidFactory $ length currentBoids
                      send boidChannel newBoids
                      pure unit
                    Nothing -> pure unit
      addEventListener click handler false $ Buttons.toEventTarget btn
      pure unit
    Nothing -> pure unit    

  case slider' of
    Just slider' -> do
      handler <- slideEvent boidChannel
      addEventListener change handler false $ Inputs.toEventTarget slider'
    Nothing -> pure unit

  case canvas of
    Just canvas -> do 
      drawCanvas boidChannel canvas
    Nothing -> pure unit    
    

drawCanvas :: Channel (List Boid) -> CanvasElement -> Effect Unit
drawCanvas boids canvas = do 
  ctx <- getContext2D canvas
  let subscription = subscribe boids
  currentBoids <- get subscription
  send boids $ iteration currentBoids
  render ctx currentBoids
  window <- window
  _ <- requestAnimationFrame (drawCanvas boids canvas) window
  pure unit
  
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
