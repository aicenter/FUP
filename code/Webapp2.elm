module Webapp exposing (main)

import Browser
import Html exposing (Html, Attribute, div, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import String exposing (toList, fromChar, fromInt)
import Char exposing (isDigit)
import List exposing (indexedMap, foldl)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict exposing (Dict, empty)

-- MAIN

main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { content : String
  , hist : Dict Char Int
  }

init : Model
init =
  { content = "" 
  , hist = empty 
  }

-- UPDATE

type Msg = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent 
      , hist = transform newContent 
      }

transform : String -> Dict Char Int
transform = toList >> List.filter isDigit >> foldl (\k -> Dict.update k add1) empty 

add1 : Maybe Int -> Maybe Int
add1 x = case x of
  Nothing -> Just 1
  Just n -> Just (n+1)
  
-- VIEW

bar : Int -> (Char, Int) -> Svg Msg
bar m (_, n) = rect  
          [ x (fromInt (m*20))
          , y (fromInt (300-n*20))
          , width "20"
          , height (fromInt (n*20))
          , fill "lightgreen"
          , stroke "black"
          , strokeWidth "2"
          ]
          []

chr : Int -> (Char, Int) -> Svg Msg
chr m (c, _) = text_
          [ x (fromInt (m*20 + 10))
          , y "320"
          , fill "black"
          , textAnchor "middle"
          ]
          [ text (fromChar c) ]

disp : (Int -> (Char, Int) -> Svg Msg) -> Model -> List (Svg Msg)
disp f = .hist >> Dict.toList >> indexedMap f 

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Enter text", value model.content, onInput Change ] []
    , div [] 
        [ svg
          [ viewBox "0 0 400 400"
          , width "400"
          , height "400"
          ]
          (disp bar model ++ disp chr model)
       ] 
    ]