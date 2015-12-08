module UI.Update where

import UI.Model exposing (..)
import AlienPanic.Update exposing (update) 

update input ui_model =
  case ui_model.game_model of
    Nothing ->
      ui_model
    Just m ->
      {
        game_model=Just (AlienPanic.Update.update input m)
      }
