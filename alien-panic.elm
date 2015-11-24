import AlienPanic.Model as Model exposing (from_tiles)
import AlienPanic.View exposing (view)
import AlienPanic.Update exposing (update)
import AlienPanic.Levels exposing (..)

import Graphics.Element exposing (..)
import Time exposing (fps)
import Signal exposing (constant, map, foldp)

main =
  case Model.from_tiles 32 level0 of
    Nothing -> constant (show "Model couldn't be loaded!")
    Just model -> Signal.map view (Signal.foldp update model input)

input = fps 60
