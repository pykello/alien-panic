import AlienPanic.Model as Model exposing (from_tiles)
import AlienPanic.View exposing (view)
import AlienPanic.Update exposing (update)
import AlienPanic.Levels exposing (..)

import Graphics.Element exposing (..)
import Time exposing (fps)
import Signal exposing (constant, map3, foldp, sampleOn)
import Keyboard exposing (arrows)

main =
  case Model.from_tiles 40.0 64 level0 of
    Nothing -> constant (show "Model couldn't be loaded!")
    Just model -> Signal.map view (Signal.foldp update model input)

input =
  let
    delta = fps 12
  in
    Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.space)
