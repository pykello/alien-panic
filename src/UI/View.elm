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
char_w = 12
line_h = 27
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
    linecount = List.length messages
    msgbox_w = char_w * maxlen + 150
    msgbox_h = line_h * linecount + 75
    msg_elems = map (contained_text msgbox_w msgbox_h) messages
  in
    collage screen_w screen_h
    (
      if messages == [] then
        []
      else
        [
          rect_i msgbox_w msgbox_h |> filled white,
          rect_i (msgbox_w-20) (msgbox_h-20) |> filled black,
          rect_i (msgbox_w-40) (msgbox_h-40) |> filled white,
          toForm (flow down msg_elems)
        ]
    )

rect_i w h =
  rect (toFloat w) (toFloat h)

contained_text w h s =
  fromString s |> Text.style message_style |>
  centered |> container w h middle
