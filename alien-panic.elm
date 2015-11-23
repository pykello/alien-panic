import AlienPanic.Model exposing (init)
import AlienPanic.View exposing (view)
import Graphics.Element exposing (..)

main =
  case init of
    Nothing -> show "Model couldn't be loaded!"
    Just model -> view model
