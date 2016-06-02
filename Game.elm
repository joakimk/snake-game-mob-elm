import Color exposing (green)
import Element exposing (toHtml)
import Collage exposing (collage, oval, filled, move)
import Html.App
import Char
import Time exposing (Time, inSeconds)
import Keyboard

view model =
  List.map drawPart model.snake
  |> collage 700 500
  |> toHtml

update msg model =
  case msg of
    Keypress code ->
      changeDirection model (Char.fromCode code)

    TimeUpdate float ->
      let
        v = (velocity model.direction)
        newSnake = (moveSnake model.snake v.x v.y)
      in
        ({ model | snake = newSnake }, Cmd.none)

changeDirection model char =
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
      { x = 1, y = 0 }
    Left ->
      { x = -1, y = 0 }
    Up ->
      { x = 0, y = 1 }
    Down ->
      { x = 0, y = -1 }

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
   |> filled green
   |> move (part.x * 20, part.y * 20)

defaultGame : Game
defaultGame =
  { snake =
    [ { x = 0, y = 0 }
    , { x = -1, y = 0 }
    , { x = -2, y = 0 }
    , { x = -3, y = 0 }
    , { x = -4, y = 0 }
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
