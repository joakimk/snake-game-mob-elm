import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (Time, inSeconds, fps)
import Window
import Keyboard

render : Game -> Element
render game =
  collage 700 500
    (List.map drawOval game.snake)

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

main : Signal Element
main =
  Signal.map render gameState

defaultGame : Game
defaultGame =
  { snake = [ (10, 10) ]
  , direction = Up
  }

type Direction = Up | Down | Left | Right
type alias Game =
  { snake : List Point
  , direction: Direction
  }

type alias Point = (Float, Float)
type alias Input =
  { directions : { x : Int, y : Int}
  , delta : Time
  }


drawOval : Point -> Form
drawOval o =
  oval 20 20
   |> filled clearGrey
   |> move o

clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6

update : Update -> Game -> Game
update updateType game =
  case updateType of
    DirectionUpdate dir ->
      updateDirection { directions = dir, delta = 0 } game
    TimeUpdate delta ->
      updateSnake { directions = { x = 0, y = 0 }, delta = delta } game

updateDirection : Input -> Game -> Game
updateDirection input game =
  let
    foo = (input.directions.x, input.directions.y)
  in
    case foo of
      (1, 0) ->
        { game | direction = Right }
      (-1, 0) ->
        { game | direction = Left }
      (0, 1) ->
        { game | direction = Up }
      (0, -1) ->
        { game | direction = Down }
      _ ->
        game

updateSnake : Input -> Game -> Game
updateSnake input game =
  let
    v = velocity game.direction
    xDir = input.delta * (toFloat v.x)
    yDir = input.delta * (toFloat v.y)
    newSnake = List.map (\(x, y) -> (x + 40 * xDir , y+ 40* yDir)) game.snake
  in
    { game | snake = newSnake }

velocity : Direction -> { x : Int, y : Int }
velocity direction =
  case direction of
    Right ->
      { x = 1, y = 0 }
    Left ->
      { x = -1, y = 0 }
    Up ->
      { x = 0, y = 1 }
    Down ->
      { x = 0, y = -1 }

delta =
  Signal.map inSeconds (fps 4)

type Update
  = DirectionUpdate { x : Int, y : Int }
  | TimeUpdate Float

input : Signal Update
input =
  Signal.merge
   (Signal.map DirectionUpdate Keyboard.wasd)
   (Signal.map TimeUpdate delta)
