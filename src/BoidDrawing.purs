module BoidDrawing where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (Context2D, fillPath, fillRect, arc, setFillStyle, getContext2D,
                        getCanvasElementById)
import Math (pi)
import Data.Int (toNumber)
import Partial.Unsafe (unsafePartial)

type State =
  { x::Number, y::Number }

main :: Effect Unit
main = void $ unsafePartial do 
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  clearCanvas ctx
  drawBoid ctx { x: 400.0, y: 300.0 }

clearCanvas :: Context2D -> Effect Unit
clearCanvas ctx = do
  setFillStyle ctx "#000000"
  fillRect ctx { x: 0.0, y: 0.0, width: 800.0, height: 600.0 }

drawBoid :: Context2D -> State -> Effect Unit
drawBoid ctx state = do
  setFillStyle ctx "tomato"
  fillPath ctx $ arc ctx {
    x: state.x,
    y: state.y,
    radius: 10.0,
    start: 0.0,
    end: toNumber 2 * pi
  }

-- Run this to build the JS-file
-- pulp build -O --main BoidDrawing --to dist/Main.js