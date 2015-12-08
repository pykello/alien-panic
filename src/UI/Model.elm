module UI.Model where

import AlienPanic.Model exposing (GameModel, from_tiles)
import AlienPanic.Levels exposing (..)

type alias UIModel = {
    game_model: Maybe GameModel    
}

init: UIModel
init =
  {game_model=from_tiles 40.0 64 level0}

