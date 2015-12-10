module UI.View where

import AlienPanic.Model exposing (GameModel)
import AlienPanic.View exposing (view)
import UI.Model exposing (..)

import Color exposing (..)
import Graphics.Collage as Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)
import List exposing (concat, map, foldl, length)
import String exposing (length)

screen_w = 9 * 64
screen_h = 12 * 64
message_style = {defaultStyle|
                  color=black,
                  height=Just 20,
                  typeface=["monospace","sans-serif"]}

view: UIModel -> Element
view ui_model =
  case ui_model.game_model of
    Nothing ->
      show "Model couldn't be loaded!"
    Just m ->
      layers [
        AlienPanic.View.view m,
        view_messages (get_messages m)
      ]

get_messages model =
  if model.lost then ["You Lost!"]
  else if model.won then ["You won!"]
  else []

view_messages: List String -> Element
view_messages messages =
  let
    maxlen = foldl (max << String.length) 0 messages
    w = 12.0 * toFloat maxlen + 150.0
    h = 75 + 24 * (toFloat (List.length messages))
    msg_elems = map
      (\s ->
        fromString s |> Text.style message_style |> centered |>
        container (round w) 24 middle)
      messages
  in
    collage screen_w screen_h
    (
      if messages == [] then
        []
      else
        [
          rect w h |> filled white,
          rect (w-20) (h-20) |> filled black,
          rect (w-40) (h-40) |> filled white,
          toForm (flow down msg_elems)
        ]
    )
