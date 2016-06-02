import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Html.App
import Time exposing (Time, inSeconds)
import Window
import Keyboard

view model =
  collage 700 500 [ drawOval (model.snake.x, model.snake.y) ]
  |> toHtml

update msg model =
  case msg of
    Keypress code ->
      let
        newSnake = { x = model.snake.x + 20, y = model.snake.y }
      in
        ({ model | snake = newSnake }, Cmd.none)

main =
  Html.App.program
  { view = view
  , update = update
  , init = (defaultGame, Cmd.none)
  , subscriptions = \_ -> (Keyboard.presses Keypress)
  }

drawOval o =
  oval 20 20
   |> filled gray
   |> move o

defaultGame : Game
defaultGame =
  { snake = { x = 0, y = 0 }
  , direction = Up
  }

type Direction = Up | Down | Left | Right
type alias Game =
  { snake : Point
  , direction: Direction
  }

type alias Point = { x : Float, y : Float }
type alias Input =
  { directions : { x : Int, y : Int}
  , delta : Time
  }

type Msg = Keypress Keyboard.KeyCode
