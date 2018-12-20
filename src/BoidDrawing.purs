module BoidDrawing where

import Prelude

import Data.Int (toNumber)
import Data.List (List(..), (:), head, (!!), length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst)
import Effect (Effect, forE)
import Effect.Console (logShow)
import Graphics.Canvas (Context2D, fillPath, fillRect, arc, setFillStyle, getContext2D, getCanvasElementById)
import Math (pi)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, discard, ($), void, (*), bind, (<$>), pure, unit)
import Vector (Vector(..), Vector)
import Boid (Boid)

-- main :: Vector Number -> Effect Unit
-- main position = void $ unsafePartial do 
--   Just canvas <- getCanvasElementById "canvas"
--   ctx <- getContext2D canvas
--   clearCanvas ctx
--   drawBoid ctx position

main :: List (Vector Number) -> Effect Unit
main positions = void $ unsafePartial do 
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  pure unit

clearCanvas :: Context2D -> Effect Unit
clearCanvas ctx = do
  setFillStyle ctx "#000000"
  fillRect ctx { x: 0.0, y: 0.0, width: 800.0, height: 600.0 }

drawBoids :: Context2D -> List Boid -> Effect Unit
drawBoids ctx boids = do 
  forE 0 (length boids) (\x -> do 
    let positions = fst <$> boids
    let position = fromMaybe (V (20.0:50.0:Nil)) (positions !! x)
    drawBoid ctx position
  )

drawBoid :: Context2D -> Vector Number -> Effect Unit
drawBoid ctx (V vector) = do
  let x = fromMaybe 400.0 $ head vector
  let y = fromMaybe 300.0 $ vector !! 1
  setFillStyle ctx "tomato"
  fillPath ctx $ arc ctx {
    x: x,
    y: y,
    radius: 3.0,
    start: 0.0,
    end: toNumber 2 * pi
  }

-- Run this to build the JS-file
-- pulp build -O --main BoidDrawing --to dist/Main.js