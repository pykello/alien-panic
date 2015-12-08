module UI.View where

import AlienPanic.Model exposing (GameModel)
import AlienPanic.View exposing (view)
import UI.Model exposing (..)

import Color exposing (..)
import Graphics.Collage as Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)
import List exposing (concat, map)

view: UIModel -> Element
view ui_model =
  case ui_model.game_model of
    Nothing ->
      show "Model couldn't be loaded!"
    Just m ->
      layers [
        AlienPanic.View.view m
      ]

