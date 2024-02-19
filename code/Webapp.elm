module Webapp exposing (main)

import Browser
import Html exposing (Html, Attribute, div, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import String exposing (toList, fromChar, fromInt)
import Char exposing (isDigit)
import List exposing (sort, indexedMap)
import Tuple exposing (first, second)
import Svg exposing (..)
import Svg.Attributes exposing (..)

-- MAIN

main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { content : String
  , hist : List (Char, Int)
  }

init : Model
init =
  { content = "" 
  , hist = [] 
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

transform : String -> List (Char, Int)
transform = toList >> List.filter isDigit >> sort >> group 

group : List Char -> List (Char, Int)
group lst = 
  case lst of
    [] -> []
    (x::xs) -> case (group xs) of
                 [] -> [(x, 1)] 
                 (t::ts) -> if first t == x then
                              (x, 1 + second t)::ts
                            else (x,1)::t::ts

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
disp f = .hist >> indexedMap f 

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