import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Html.App
import Char
import Time exposing (Time, inSeconds)
import Window
import Keyboard

view model =
  collage 700 500 (List.map drawPart model.snake)
  |> toHtml

update msg model =
  case msg of
    Keypress code ->
      handleKeyPress model (Char.fromCode code)

    TimeUpdate float ->
      let
        v = (velocity model.direction)
        newSnake = (moveSnake model.snake v.x v.y)
      in
        ({ model | snake = newSnake }, Cmd.none)

handleKeyPress model char =
  case char of
    'a' ->
      ({ model | direction = Left }, Cmd.none)
    'w' ->
      ({ model | direction = Up }, Cmd.none)
    's' ->
      ({ model | direction = Down }, Cmd.none)
    'd' ->
      ({ model | direction = Right }, Cmd.none)
    _ ->
      (model, Cmd.none)

velocity : Direction -> { x : Float, y : Float }
velocity direction =
  case direction of
    Right ->
      { x = 20, y = 0 }
    Left ->
      { x = -20, y = 0 }
    Up ->
      { x = 0, y = 20 }
    Down ->
      { x = 0, y = -20 }

moveSnake snake x y =
  let
    -- we assume there is always a head, so the maybe won't trigger
    oldHead =
      List.head snake
      |> (Maybe.withDefault { x = 0, y = 0 })

    newHead = { x = oldHead.x + x, y = oldHead.y + y }
  in
    List.append [ newHead ] snake
    |> List.reverse
    |> List.drop 1
    |> List.reverse

main =
  Html.App.program
  { view = view
  , update = update
  , init = (defaultGame, Cmd.none)
  , subscriptions = subscriptions
  }

subscriptions _ =
 [ (Keyboard.presses Keypress)
 , (Time.every (Time.millisecond * 250) TimeUpdate)
 ]
 |> Sub.batch

drawPart part =
  oval 20 20
   |> filled gray
   |> move (part.x, part.y)

defaultGame : Game
defaultGame =
  { snake =
    [ { x = 0, y = 0 }
    , { x = -20, y = 0 }
    , { x = -40, y = 0 }
    , { x = -60, y = 0 }
    , { x = -80, y = 0 }
    ]
  , direction = Up
  }

type Direction = Up | Down | Left | Right
type alias Game =
  { snake : List Part
  , direction: Direction
  }

type alias Part = { x : Float, y : Float }

type Msg = Keypress Keyboard.KeyCode
         | TimeUpdate Float
