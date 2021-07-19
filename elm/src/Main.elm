module Main exposing (..)
import Browser
import Html exposing (Html, Attribute, table, tbody, tr, td, div, button, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time
import Array exposing (get, set, fromList, toList, Array)
import Random
import Random.Array
import Basics

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- MODEL

type alias Model =
  { game : Array Bool,
    running: Bool}

createGame settings =
  Array.repeat (settings.rows * settings.cols) False

gameSettings =
  {rows = 30,
   cols = 50}

init : () -> (Model, Cmd Msg)
init _ =
  ({ game = (createGame gameSettings),
    running = True},
    Cmd.none)

-- Data structure manipulation

toX : Int -> Int
toX index =
  (index // gameSettings.rows)

toY : Int -> Int
toY index =
  (modBy gameSettings.rows index)

toIndex : Int -> Int -> Int
toIndex x y =
  gameSettings.rows * x  +y

toMatrix : Array Bool -> List (List Bool)
toMatrix list =
  List.indexedMap (\_ indexX ->
    List.indexedMap (\_ indexY ->
      case (getCell list indexX indexY) of
        Just a -> a
        Nothing -> False
    ) (List.range 0 gameSettings.cols)
  ) (List.range 0 gameSettings.rows)

getCell : Array a -> Int -> Int -> Maybe a
getCell list x y =
  get (toIndex x y) list


setCellFn : Array a -> Int -> Int -> (a -> a) -> a -> Array a
setCellFn list x y fn default =
  set (toIndex x y) (fn (
    case (getCell list x y) of
      Just a ->
        a
      Nothing ->
        default
    )) list

setCellFnBool list x y fn=
  setCellFn list x y fn False


-- UPDATE

type Msg =
  Flip Int Int |
  NextState |
  GenerateRandomBoard |
  RandomBoard (Array Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Flip x y ->
      ({model | game = (setCellFnBool model.game x y not) }, Cmd.none)

    NextState ->
      ({model | game = (updateTable model.game)}, Cmd.none)

    GenerateRandomBoard ->
      (model, Random.generate RandomBoard (Random.Array.array (gameSettings.cols * gameSettings.rows) (Random.int 1 100)))

    RandomBoard randArray ->
      ({model | game = (Array.indexedMap (\index _ ->
          case (get index randArray) of
            Just a -> a < 30
            Nothing -> False
        ) (Array.repeat ((gameSettings.rows * gameSettings.cols) - 1) 0))}, Cmd.none)

updateTable : Array Bool -> Array Bool
updateTable table =
  Array.indexedMap (\index cell -> updateCell (toX index) (toY index) cell table) table

updateCell : Int -> Int -> Bool -> Array Bool -> Bool
updateCell x y cell table =
  let
    neighbourTranslations = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    neighboursMaybe = List.map (\(i, j) -> getCell table (x+i) (y+j)) neighbourTranslations
    neighbours = List.filterMap identity neighboursMaybe
    neighboursInt = List.map (\el -> if el then 1 else 0) neighbours
    count = List.foldl (+) 0 neighboursInt
    newState = if cell then
        count == 2 || count == 3
      else
        count == 3
   in
    newState


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running
        then Time.every 300 (\_ -> NextState)
        else Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    table [] [ tbody [] (renderTable model)]
    , button [ onClick GenerateRandomBoard ] [ text "Random" ]
  ]

renderTable : Model -> List (Html Msg)
renderTable model =
  (List.indexedMap (renderRow) (toMatrix model.game))

renderRow : Int -> List Bool -> Html Msg
renderRow x row =
  tr [] (List.indexedMap (renderCell x) row)

renderCell : Int -> Int -> Bool -> Html Msg
renderCell x y cell =
  td (List.append (styles [Cell, if cell then Alive else NoStyle]) [onClick (Flip x y)]) []


-- Styles
type Styles =
  Alive |
  Cell |
  NoStyle

myStyle : Styles -> List (Attribute msg)
myStyle s =
  case s of
    Alive ->
      [ style "backgroundColor" "black" ]
    Cell ->
      [ style "width" "20px", style "height" "20px", style "border" "1px black solid" ]
    NoStyle ->
      []

styles : List Styles -> List (Attribute msg)
styles s =
  List.concat (List.map myStyle s)