module BoidDrawing where

import Prelude

import Data.Int (toNumber)
import Data.List (List(..), (:), head, (!!), length)
import Data.Maybe (fromMaybe)
import Data.Tuple (fst)
import Effect (Effect, forE)
import Graphics.Canvas (Context2D, arc, fillPath, fillRect, setFillStyle)
import Math (pi) 
import Prelude (Unit, discard, ($), (*), (<$>))
import Vector (Vector(..), Vector)
import Boid (Boid)

clearCanvas :: Context2D -> Effect Unit
clearCanvas ctx = do
  setFillStyle ctx "#000000"
  fillRect ctx { x: 0.0, y: 0.0, width: 1280.0, height: 940.0 }

drawBoids :: Context2D -> List Boid -> Effect Unit
drawBoids ctx boids = do 
  forE 0 (length boids) (\x -> do 
    let positions = fst <$> boids
    let position = fromMaybe (V (20.0:50.0:Nil)) (positions !! x)
    drawBoid ctx position
  )

drawBoid :: Context2D -> Vector Number -> Effect Unit
drawBoid ctx (V (x:y:Nil)) = do
  setFillStyle ctx "lightgreen"
  fillPath ctx $ arc ctx {
    x: x,
    y: y,
    radius: 3.0,
    start: 0.0,
    end: toNumber 2 * pi
  }
drawBoid ctx _ = pure unit
-- Run this to build the JS-file
-- pulp build -O --main BoidDrawing --to dist/Main.js