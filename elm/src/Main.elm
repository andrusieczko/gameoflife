module Main exposing (..)
import Browser
import Html exposing (Html, Attribute, table, tbody, tr, td)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- MODEL

type alias Model =
  { game : List (List Bool),
    running: Bool }


createRows settings =
  List.repeat settings.rows False

createCols settings =
  List.repeat settings.cols False

createGame settings =
  List.map (\_ -> createCols settings) (createRows settings)

gameSettings =
  {rows = 30,
   cols = 30}

init : () -> (Model, Cmd Msg)
init _ =
  ({ game = (createGame gameSettings),
    running = True },
    Cmd.none)


-- list operations

set : List a -> Int -> a -> List a
set list i x =
  List.indexedMap (\index el -> if index == i then x else el) list

setFn : List a -> Int -> (a -> a) -> List a
setFn list i fn =
  List.indexedMap (\index el -> if index == i then (fn el) else el) list

set2 : List (List a) -> Int -> Int -> a -> List (List a)
set2 list x y value =
  List.indexedMap (\index row -> if index == x then (set row y value) else row) list

set2Fn : List (List a) -> Int -> Int -> (a -> a) -> List (List a)
set2Fn list x y fn =
  List.indexedMap (\index row -> if index == x then (setFn row y fn) else row) list

-- UPDATE


type Msg =
  Flip Int Int |
  NextState
  


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Flip x y ->
      ({model | game = (set2Fn model.game x y not) }, Cmd.none)

    NextState ->
      ({ model | game = (set2Fn model.game 1 1 not)}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running
        then Time.every 1000 (\_ -> NextState)
        else Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  table [] [ tbody [] (renderTable model)]

renderTable : Model -> List (Html Msg)
renderTable model =
  (List.indexedMap (renderRow) model.game)

renderRow : Int -> List Bool -> Html Msg
renderRow x row =
  tr [] (List.indexedMap (renderCell x) row)

renderCell : Int -> Int -> Bool -> Html Msg
renderCell x y cell =
  td (List.append (styles [Cell, if cell then Alive else Nothing]) [onClick (Flip x y)]) []


-- Styles
type Styles =
  Alive |
  Cell |
  Nothing

myStyle : Styles -> List (Attribute msg)
myStyle s =
  case s of
    Alive ->
      [ style "backgroundColor" "black" ]
    Cell ->
      [ style "width" "20px", style "height" "20px", style "border" "1px black solid" ]
    Nothing ->
      []

styles : List Styles -> List (Attribute msg)
styles s =
  List.concat (List.map myStyle s)