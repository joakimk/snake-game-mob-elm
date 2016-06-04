-- Some helpers to render to a fullscreen grid beginning in the top left (0, 0).

module Grid exposing (windowPosition, gridSize, gridMove)

import Window
import Collage exposing (move, Form)

windowPosition : (Float, Float) -> Window.Size -> (Float, Float)
windowPosition (x, y) windowSize =
  let
    windowX = x * gridSize    - (toFloat windowSize.width / 2)
    windowY = -(y * gridSize) + (toFloat windowSize.height / 2) - gridSize
  in
    (windowX, windowY)

gridMove : Float -> Float -> Window.Size -> Form -> Form
gridMove x y windowSize form =
  form
  |> move (windowPosition (x, y) windowSize)

gridSize = 32
