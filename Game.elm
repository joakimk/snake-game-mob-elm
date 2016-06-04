import Color exposing (green, grayscale)
import Element exposing (toHtml)
import Collage exposing (collage, oval, rect, filled, move, group, Form)
import Html.App
import Html exposing (Html)
import Time exposing (Time, inSeconds)
import Keyboard
import Window
import Task

-- VIEW

view : Game -> Html Msg
view game =
  collage game.window.width game.window.height [
    drawBackground game
  , drawSnake game.snake
  ]
  |> toHtml

drawBackground : Game -> Form
drawBackground game =
  rect (toFloat game.window.width) (toFloat game.window.height)
  |> filled (grayscale 0.8)

drawSnake : Snake -> Form
drawSnake snake =
  List.map drawPart snake
  |> group

drawPart : Part -> Form
drawPart part =
  oval 20 20
   |> filled green
   |> move (part.x * 20, part.y * 20)

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

updateByKeyboardCharacter : Game -> Char -> (Game, Cmd a)
updateByKeyboardCharacter model char =
  case char of
    'a' ->
      (model, Cmd.none)
    _ ->
      (model, Cmd.none)

-- MODEL

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
  , window = { width = 0, height = 0 }
  }

type Direction = Up | Down | Left | Right
type alias Game =
  { snake : Snake
  , direction: Direction
  , window : Window.Size
  }

type alias Snake = List Part
type alias Part = { x : Float, y : Float }

type Msg = Keypress Keyboard.KeyCode
         | TimeUpdate Float
         | WindowResize Window.Size
-- Glue

main : Program Never
main =
  Html.App.program
  { view = view
  , update = update
  , init = (defaultGame, issueInitialWindowSizeMsg)
  , subscriptions = subscriptions
  }

issueInitialWindowSizeMsg =
  Task.perform WindowResize WindowResize (Window.size)

subscriptions : a -> Sub Msg
subscriptions _ =
  [ (Keyboard.presses Keypress)
  , (Time.every (Time.millisecond * 250) TimeUpdate)
  , (Window.resizes WindowResize)
  ]
  |> Sub.batch
