module AlienPanic.Update where

import AlienPanic.Model exposing (..)

update: Float -> GameModel -> GameModel
update delta model =
  {model| player=move (0.5 * delta * 0.001) model.player}

move dx obj =
  let
    (x, y) = obj.pos
  in
    {obj|pos=(x+dx, y), dir=RIGHT, verb="walking"}
