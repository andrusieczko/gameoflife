module Main exposing (..)
import Browser
import Html exposing (Html, Attribute, table, tbody, tr, td, div, button, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time
import Array exposing (get, toList, Array)
import Random
import Random.Array
import Basics

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- MODEL

type alias Model =
  { game : Array (Array Bool),
    running: Bool}

createRows settings =
  Array.repeat settings.rows False

createCols settings =
  Array.repeat settings.cols False

createGame settings =
  Array.map (\_ -> createCols settings) (createRows settings)

gameSettings =
  {rows = 30,
   cols = 50}

init : () -> (Model, Cmd Msg)
init _ =
  ({ game = (createGame gameSettings),
    running = True},
    Cmd.none)


-- list operations

setFn : Array a -> Int -> (a -> a) -> Array a
setFn list i fn =
  Array.indexedMap (\index el -> if index == i then (fn el) else el) list

set2Fn : Array (Array a) -> Int -> Int -> (a -> a) -> Array (Array a)
set2Fn list x y fn =
  Array.indexedMap (\index row -> if index == x then (setFn row y fn) else row) list

get2 : Array (Array a) -> Int -> Int -> Maybe a
get2 list x y =
  case get x list of
    Just row ->
      get y row
    Nothing ->
      Nothing

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
      ({model | game = (set2Fn model.game x y not) }, Cmd.none)

    NextState ->
      ({model | game = (updateTable model.game)}, Cmd.none)

    GenerateRandomBoard ->
      (model, Random.generate RandomBoard (Random.Array.array (gameSettings.cols * gameSettings.rows) (Random.int 1 100)))

    RandomBoard randArray ->
      ({model | game = (Array.indexedMap (\x _ ->
          Array.indexedMap (\y _ ->
            case (get (x*gameSettings.rows + y) randArray) of
              Just a -> a < 30
              Nothing -> False
          ) (Array.repeat (gameSettings.cols - 1) 0)
        ) (Array.repeat (gameSettings.rows - 1) 0))}, Cmd.none)

updateTable : Array (Array Bool) -> Array (Array Bool)
updateTable table =
  Array.indexedMap (\index row -> updateRow index row table) table

updateRow : Int -> Array Bool -> Array (Array Bool) -> Array Bool
updateRow x row table =
  Array.indexedMap (\y cell -> updateCell x y cell table) row

updateCell : Int -> Int -> Bool -> Array (Array Bool) -> Bool
updateCell x y cell table =
  let
    neighbourTranslations = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    neighboursMaybe = List.map (\(i, j) -> get2 table (x+i) (y+j)) neighbourTranslations
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
  (toList (Array.indexedMap (renderRow) model.game))

renderRow : Int -> Array Bool -> Html Msg
renderRow x row =
  tr [] (toList (Array.indexedMap (renderCell x) row))

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