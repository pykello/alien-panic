module UI.View where

import AlienPanic.Model exposing (GameModel)
import AlienPanic.View exposing (view)
import UI.Model exposing (..)

import Color exposing (..)
import Graphics.Collage as Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)
import List exposing (concat, map)

message_style = {defaultStyle|
                  color=white,
                  height=Just 20,
                  typeface=["arial","sans-serif"]}

view: UIModel -> Element
view ui_model =
  case ui_model.game_model of
    Nothing ->
      show "Model couldn't be loaded!"
    Just m ->
      layers [
        AlienPanic.View.view m,
        view_messages m
      ]

view_messages: GameModel -> Element
view_messages model =
  let
    screen = model.screen
    sw = screen.width * screen.unit
    sh = screen.width * screen.unit
    w = sw * 0.5
    h = 100.0
    message = if model.lost then "You Lost!"
              else if model.won then "You won!"
              else ""
  in
    collage (floor sw) (floor sh)
    (
      if message == "" then
        []
      else
        [
          rect w h |> filled (rgb 100 100 100),
          text (fromString message |> Text.style message_style)
        ]
    )
