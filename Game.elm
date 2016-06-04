import Color exposing (green)
import Element exposing (toHtml)
import Collage exposing (collage, oval, filled, move, Form)
import Html.App
import Html exposing (Html)
import Time exposing (Time, inSeconds)
import Keyboard

-- VIEW

view : Game -> Html Msg
view game =
  drawSnake game.snake
  |> collage 1000 500
  |> toHtml

drawSnake : Snake -> List Form
drawSnake snake =
  List.map drawPart snake

drawPart : Part -> Form
drawPart part =
  oval 20 20
   |> filled green
   |> move (part.x * 20, part.y * 20)

-- UPDATE

update : Msg -> Game -> (Game, Cmd a)
update msg model =
  case msg of
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
  }

type Direction = Up | Down | Left | Right
type alias Game =
  { snake : Snake
  , direction: Direction
  }

type alias Snake = List Part
type alias Part = { x : Float, y : Float }

type Msg = Keypress Keyboard.KeyCode
         | TimeUpdate Float
-- Glue

main : Program Never
main =
  Html.App.program
  { view = view
  , update = update
  , init = (defaultGame, Cmd.none)
  , subscriptions = subscriptions
  }

subscriptions : a -> Sub Msg
subscriptions _ =
 [ (Keyboard.presses Keypress)
 , (Time.every (Time.millisecond * 250) TimeUpdate)
 ]
 |> Sub.batch
