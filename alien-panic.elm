import UI.Model exposing (..)
import UI.Update exposing (..)
import UI.View exposing (..)

import Time exposing (fps)
import Signal exposing (constant, map3, foldp, sampleOn)
import Keyboard exposing (arrows)

main =
  Signal.map view (Signal.foldp update init input)

input =
  let
    delta = fps 12
  in
    Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.space)
