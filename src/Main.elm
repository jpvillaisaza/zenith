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


type Model
  = Input1 String
  | Input2 String
  | Input3 String
  | Input4 String


init : Model
init =
  Input1 ""


type Msg
  = Update1 String
  | Update2 String
  | Update3 String
  | Update4 String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Update1 dirty ->
      let m = clean dirty in Input1 m

    Update2 dirty ->
      let m = clean dirty in Input2 m

    Update3 dirty ->
      let m = clean dirty in Input3 m

    Update4 dirty ->
      let m = clean dirty in Input4 m


clean : String -> String
clean =
  let
    p n acc xs =
      case (n, xs) of
        (0, []) -> String.reverse acc
        (_, (','::ss)) -> p n acc ('.'::ss)
        (0, (s::ss)) -> if s == '.' then p 1 (String.cons s acc) ss else p 0 (String.cons s acc) ss
        (m, []) -> String.reverse acc
        (m, (s::ss)) -> if s == '.' then String.reverse acc else p m (String.cons s acc) ss
  in
    p 0 "" << String.toList << String.filter (\c -> Char.isDigit c || c == '.' || c == ',')


view : Model -> Html Msg
view model =
  let
    (input1, (input2, input3, input4)) =
      case model of
        Input1 i1 -> let v = String.toFloat i1 in
          ( i1
          , (Maybe.withDefault "" (Maybe.map (truncate << l100ToMpg) v)
          , Maybe.withDefault "" (Maybe.map (truncate << milesToKilometers << l100ToMpg) v)
          , Maybe.withDefault "" (Maybe.map (truncate << l100ToKpl) v))
          )

        Input2 i2 -> let v = String.toFloat i2 in
          ( Maybe.withDefault "" (Maybe.map (truncate << mpgToL100) v)
          , (i2
          , Maybe.withDefault "" (Maybe.map (truncate << milesToKilometers) v)
          , Maybe.withDefault "" (Maybe.map (truncate << l100ToKpl << mpgToL100) v))
          )

        Input3 i3 -> let v = String.toFloat i3 in
          ( Maybe.withDefault "" (Maybe.map (truncate << mpgToL100 << kilometersToMiles) v)
          , (Maybe.withDefault "" (Maybe.map (truncate << kilometersToMiles) v)
          , i3
          , Maybe.withDefault "" (Maybe.map (truncate << l100ToKpl << mpgToL100 << kilometersToMiles) v))
          )

        Input4 i4 -> let v = String.toFloat i4 in
          ( Maybe.withDefault "" (Maybe.map (truncate << kplToL100) v)
          , (Maybe.withDefault "" (Maybe.map (truncate << l100ToMpg << kplToL100) v)
          , Maybe.withDefault "" (Maybe.map (truncate << milesToKilometers << l100ToMpg << kplToL100) v)
          , i4)
          )
  in
    Html.form
      [ Html.Attributes.style "font-family" "sans-serif"
      ]
      [ Html.fieldset
        [ Html.Attributes.style "margin" "16px auto"
        , Html.Attributes.style "min-width" "128px"
        , Html.Attributes.style "max-width" "256px"
        ]
        [ Html.legend []
          [ Html.text "Units of fuel per fixed distance" ]
        , viewI "fuel-consumption" True [text "L/100 km"] Update1 input1
        ]
      , Html.fieldset
        [ Html.Attributes.style "margin" "16px auto"
        , Html.Attributes.style "min-width" "128px"
        , Html.Attributes.style "max-width" "256px"
        ]
        [ Html.legend []
          [ Html.text "Units of distance per fixed fuel unit" ]
        , viewI "fuel-economy-mpg" False [text "mpg", sub [] [text "US"]] Update2 input2
        , viewI "fuel-economy-kpg" False [text "kpg", sub [] [text "US"]] Update3 input3
        , viewI "fuel-economy-kpl" False [text "km/L"] Update4 input4
        ]
      ]


viewI i af l u v =
  div
    [ Html.Attributes.style "display" "flex"
    , Html.Attributes.style "flex-direction" "column"
    , Html.Attributes.style "margin-top" "16px"
    , Html.Attributes.style "margin-bottom" "16px"
    ]
    [ label
      [ Html.Attributes.for i
      , Html.Attributes.style "text-align" "center"
      ]
      l
    , input
      [ autofocus af
      , Html.Attributes.class "form-input"
      , Html.Attributes.id i
      , Html.Attributes.attribute "inputmode" "decimal"
      , onInput u
      , Html.Attributes.style "font-size" "32px"
      , Html.Attributes.style "margin" "0 auto"
      , Html.Attributes.style "min-width" "128px"
      , Html.Attributes.style "max-width" "80%"
      , Html.Attributes.style "text-align" "center"
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


l100ToKpl : Float -> Float
l100ToKpl n =
  100 / n


kplToL100 : Float -> Float
kplToL100 n =
  100 / n


truncate : Float -> String
truncate f =
  case String.split "." (String.fromFloat f) of
      a :: b :: _ ->
        a ++ "." ++ String.left 2 b
      a :: _ ->
        a
      _ ->
        ""
