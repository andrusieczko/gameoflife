module Main exposing (..)
import Browser
import Html exposing (Html, Attribute, div, text, table, tbody, tr, td)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { game : List (List Bool)}


createRows settings =
  List.repeat settings.rows False

createCols settings =
  List.repeat settings.cols False

createGame settings =
  List.map (\_ -> createCols settings) (createRows settings)

gameSettings =
  {rows = 30,
   cols = 30}

init : Model
init =
  { game = (createGame gameSettings)}


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


type Msg
  = Flip Int Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Flip x y ->
        {model | game = (set2Fn model.game x y not) }

-- VIEW

view : Model -> Html Msg
view model =
  table [] [ tbody [] (renderTable model)]

renderTable : Model -> List (Html Msg)
renderTable model =
  (List.indexedMap (renderRow model) model.game)

renderRow : Model -> Int -> List Bool -> Html Msg
renderRow model x row =
  tr [] (List.indexedMap (renderCell model x) row)

renderCell : Model -> Int -> Int -> Bool -> Html Msg
renderCell model x y cell =
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