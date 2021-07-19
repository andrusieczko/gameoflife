module Main exposing (..)
import Browser
import Html exposing (Html, Attribute, table, tbody, tr, td, div, button, text, label, input, h1)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    running: Bool,
    refreshInterval: String,
    randomFactor: String}

createRows settings =
  Array.repeat settings.rows False

createCols settings =
  Array.repeat settings.cols False

createGame settings =
  Array.map (\_ -> createCols settings) (createRows settings)

gameSettings =
  {rows = 30,
   cols = 50,
   refreshInterval = 300,
   randomFactor = 0.2}

init : () -> (Model, Cmd Msg)
init _ =
  ({ game = (createGame gameSettings),
    running = False,
    refreshInterval = (String.fromFloat gameSettings.refreshInterval),
    randomFactor = (String.fromFloat gameSettings.randomFactor)},
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


d : Maybe a -> a -> a
d value defaultValue =
  case value of
    Just a ->
      a
    Nothing ->
      defaultValue

-- UPDATE


type Msg =
  Flip Int Int |
  NextState |
  Start |
  Stop |
  Clear |
  UpdateRefreshInterval String |
  UpdateRandomFactor String |
  GenerateRandomBoard |
  RandomBoard (Array Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Flip x y ->
      ({model | game = (set2Fn model.game x y not) }, Cmd.none)

    NextState ->
      ({model | game = (updateTable model.game)}, Cmd.none)

    Start ->
      ({model | running = True}, Cmd.none)

    Stop ->
      ({model | running = False}, Cmd.none)

    Clear ->
      ({model | game = (createGame gameSettings)}, Cmd.none)

    UpdateRefreshInterval refreshInterval ->
      ({model | refreshInterval = refreshInterval}, Cmd.none)

    UpdateRandomFactor randomFactor ->
      ({model | randomFactor = randomFactor}, Cmd.none)

    GenerateRandomBoard ->
      (model, Random.generate RandomBoard (Random.Array.array (gameSettings.cols * gameSettings.rows) (Random.int 1 100)))

    RandomBoard randArray ->
      ({model | game = (Array.indexedMap (\x _ ->
          Array.indexedMap (\y _ ->
            case (get (x*gameSettings.rows + y) randArray) of
              Just a -> a < floor (d (String.toFloat model.randomFactor) gameSettings.randomFactor * 100)
              Nothing -> False
          ) (Array.repeat (gameSettings.cols) 0)
        ) (Array.repeat (gameSettings.rows) 0))}, Cmd.none)

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
        then Time.every (d (String.toFloat model.refreshInterval) gameSettings.refreshInterval) (\_ -> NextState)
        else Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    h1 [] [text "Elm"],
    table [] [ tbody [] (renderTable model)]
    , button [ onClick Start ] [ text "Start " ]
    , button [ onClick Stop ] [ text "Stop " ]
    , button [ onClick GenerateRandomBoard ] [ text "Random " ]
    , button [ onClick Clear ] [ text "Clear " ]
    , text " | "
    , label [ for "refreshInterval" ] [ text "Interval: " ]
    , input [ type_ "text", id "refreshInterval", value model.refreshInterval, onInput UpdateRefreshInterval ] [ ]
    , text " | "
    , label [ for "randomFactor" ] [ text "Random factor: " ]
    , input [ type_ "text", id "randomFactor", value model.randomFactor, onInput UpdateRandomFactor ] [ ]
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