module UI.Update where

import UI.Model exposing (..)
import AlienPanic.Update as GameUpdate exposing (update)

update (delta, arrows, space) ui_model =
  case ui_model.game_model of
    Nothing ->
      ui_model
    Just m ->
      if not m.won && not m.lost then
        {ui_model| game_model=Just (GameUpdate.update (delta, arrows, space) m)}
      else if m.lost && space then
        init
      else
        ui_model
