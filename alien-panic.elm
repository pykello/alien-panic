import AlienPanic.Model as Model exposing (from_tiles)
import AlienPanic.View exposing (view)
import AlienPanic.Levels exposing (..)
import Graphics.Element exposing (..)

main =
  case Model.from_tiles 32 level0 of
    Nothing -> show "Model couldn't be loaded!"
    Just model -> view model
