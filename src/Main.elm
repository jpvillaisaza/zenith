module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }


type alias Model =
  { input1 : { i : String, v : Maybe Float }
  , input2 : { i : String, v : Maybe Float }
  , input3 : { i : String, v : Maybe Float }
  }


init : Model
init =
  { input1 = { i = "", v = Nothing }
  , input2 = { i = "", v = Nothing }
  , input3 = { i = "", v = Nothing }
  }


type Msg
  = Update1 String
  | Update2 String
  | Update3 String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Update1 dirty ->
      let m = clean dirty in
      { model
      | input1 = { i = m, v = String.toFloat m }
      , input2 = { i = Maybe.withDefault "" (Maybe.map (String.fromFloat << l100ToMpg) (String.toFloat m)), v = Maybe.map l100ToMpg (String.toFloat m) }
      , input3 = { i = Maybe.withDefault "" (Maybe.map (String.fromFloat << milesToKilometers << l100ToMpg) (String.toFloat m)), v = Maybe.map (milesToKilometers << l100ToMpg) (String.toFloat m) }
      }

    Update2 dirty ->
      let m = clean dirty in
      { model
      | input1 = { i = Maybe.withDefault "" (Maybe.map (String.fromFloat << mpgToL100) (String.toFloat m)), v = Maybe.map mpgToL100 (String.toFloat m) }
      , input2 = { i = m, v = String.toFloat m }
      , input3 = { i = Maybe.withDefault "" (Maybe.map (String.fromFloat << milesToKilometers) (String.toFloat m)), v = Maybe.map milesToKilometers (String.toFloat m) }
      }

    Update3 dirty ->
      let m = clean dirty in
      { model
      | input1 = { i = Maybe.withDefault "" (Maybe.map (String.fromFloat << mpgToL100 << kilometersToMiles) (String.toFloat m)), v = Maybe.map (mpgToL100 << kilometersToMiles) (String.toFloat m) }
      , input2 = { i = Maybe.withDefault "" (Maybe.map (String.fromFloat << kilometersToMiles) (String.toFloat m)), v = Maybe.map kilometersToMiles (String.toFloat m) }
      , input3 = { i = clean m, v = String.toFloat m }
      }


clean : String -> String
clean =
  let
    p n acc xs =
      case (n, xs) of
        (0, []) -> String.reverse acc
        (0, (s::ss)) -> if s == '.' then p 1 (String.cons s acc) ss else p 0 (String.cons s acc) ss
        (m, []) -> String.reverse acc
        (m, (s::ss)) -> if s == '.' then String.reverse acc else p m (String.cons s acc) ss
  in
    p 0 "" << String.toList << String.filter (\c -> Char.isDigit c || c == '.')


view : Model -> Html Msg
view model =
  Html.form []
    [ Html.fieldset [ Html.Attributes.class "form-group" ]
      [ Html.legend []
        [ Html.text "Fuel consumption" ]
      , viewI "fuel-consumption" True "L/100 km" Update1 model.input1.i
      ]
    , Html.fieldset [ Html.Attributes.class "form-group" ]
      [ Html.legend []
        [ Html.text "Fuel economy" ]
      , viewI "fuel-economy-mpg" False "mpg" Update2 model.input2.i
      , viewI "fuel-economy-kpg" False "kpg" Update3 model.input3.i
      ]
    ]


viewI i af l u v =
  div []
    [ label [ Html.Attributes.for i ] [ text l ]
    , input
      [ autofocus af
      , Html.Attributes.class "form-input"
      , Html.Attributes.id i
      , Html.Attributes.attribute "inputmode" "decimal"
      , onInput u
      , type_ "text"
      , value v
      ]
      []
    ]


l100ToMpg : Float -> Float
l100ToMpg n =
  235.215/n


mpgToL100 : Float -> Float
mpgToL100 n =
  235.215 / n


milesToKilometers : Float -> Float
milesToKilometers n =
  n * 1.609


kilometersToMiles : Float -> Float
kilometersToMiles n =
  n / 1.609
