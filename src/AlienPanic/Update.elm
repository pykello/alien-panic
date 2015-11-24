module AlienPanic.Update where

import AlienPanic.Model exposing (..)
import Keyboard exposing (..)

type alias Keys = { x:Int, y:Int }

update: (Float, Keys) -> GameModel -> GameModel
update (delta, keys) model =
  {model|
    player=update_player (delta, keys) model.screen model.player}

update_player (delta, keys) screen player =
  let
    dir =
      if keys.x > 0 then RIGHT
      else if keys.x < 0 then LEFT
      else player.dir
    (x, y) = player.pos
    walking = (keys.x == 1 && x < screen.width - 1.0) ||
              (keys.x == -1 && x > 0)
    dx = if walking then 0.00075 * delta * (toFloat keys.x) else 0
    verb = if walking then "walking" else ""
  in
    {player|
      pos=(x + dx, y),
      dir=dir,
      verb=verb
    }

move: Float -> GameObject -> GameObject
move dx obj =
  let
    (x, y) = obj.pos
  in
    {obj|pos=(x+dx, y), dir=RIGHT, verb="walking"}
