import Color exposing (green, grayscale)
import Element exposing (toHtml)
import Collage exposing (collage, oval, rect, filled, move, group, Form)
import Html.App
import Html exposing (Html)
import Time exposing (Time, inSeconds)
import Keyboard
import Task
import Window

import Grid exposing (gridSize, gridMove)

-- VIEW

view : Game -> Html Msg
view game =
  collage game.window.width game.window.height [
--    drawBackground game
    drawSnake game
  ]
  |> toHtml

drawSnake : Game -> Form
drawSnake game =
  List.map (drawPart game) game.snake
  |> group

drawPart : Game -> Part -> Form
drawPart game part =
  oval gridSize gridSize
  |> filled green
  |> gridMove part.x part.y game.window

--drawBackground : Game -> Form
--drawBackground game =
--  rect (toFloat game.window.width) (toFloat game.window.height)
--  |> filled (grayscale 0.8)

-- UPDATE

update : Msg -> Game -> (Game, Cmd a)
update msg model =
  case msg of
    WindowResize size ->
      ({ model | window = size }, Cmd.none)
    --Keypress code ->
    --  updateByKeyboardCharacter model (Char.fromCode code)
    _ ->
      (model, Cmd.none)

--updateByKeyboardCharacter : Game -> Char -> (Game, Cmd a)
--updateByKeyboardCharacter model char =
--  case char of
--    'a' ->
--      (model, Cmd.none)
--    _ ->
--      (model, Cmd.none)

-- MODEL

initialGame : Game
initialGame =
  { snake = [
      { x = 10, y = 5 }
    , { x = 9, y = 5 }
    , { x = 8, y = 5 }
    , { x = 7, y = 5 }
    , { x = 6, y = 5 }
    ]
  , direction = Up
  , window = { width = 0, height = 0 }
  }

type alias Game =
  { snake : Snake
  , direction: Direction
  , window : Window.Size
  }

type alias Snake = List Part
type alias Part = { x : Float, y : Float }

type Direction = Up | Down | Left | Right

-- GLUE

type Msg = Keypress Keyboard.KeyCode
         | TimeUpdate Float
         | WindowResize Window.Size

main : Program Never
main =
  Html.App.program
  { view = view
  , update = update
  , init = (initialGame, initialCommand)
  , subscriptions = subscriptions
  }

initialCommand =
  [ (Task.perform WindowResize WindowResize (Window.size))
  ]
  |> Cmd.batch

subscriptions : a -> Sub Msg
subscriptions _ =
  [ (Keyboard.presses Keypress)
  , (Time.every (Time.millisecond * 250) TimeUpdate)
  , (Window.resizes WindowResize)
  ]
  |> Sub.batch
