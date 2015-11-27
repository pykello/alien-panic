module AlienPanic.Update where

import AlienPanic.Model exposing (..)
import Keyboard exposing (..)

type alias Keys = { x:Int, y:Int }

update: (Float, Keys) -> GameModel -> GameModel
update (delta, keys) model =
  {model|
    player=update_player (delta, keys) model model.player}

update_player: (Float, Keys) -> GameModel -> GameObject -> GameObject
update_player (delta, keys) model player =
  {player| verb=""}
    |> update_dir keys 
    |> walk delta keys model
    |> climb delta model

update_dir: Keys -> GameObject -> GameObject
update_dir keys obj =
  {obj| dir = case keys.x of
                -1 -> LEFT
                1  -> RIGHT
                _  -> obj.dir}

walk: Float -> Keys -> GameModel -> GameObject -> GameObject
walk delta keys model obj =
  let
    (px, py) = obj.pos
    walking = (obj.dir == LEFT || obj.dir == RIGHT) &&
              ((keys.x == 1 && px < model.screen.width - 1.0) ||
               (keys.x == -1 && px > 0))
    dx = if walking then 0.00075 * delta * (toFloat keys.x) else 0
    x = px + dx
    y = if walking then toFloat (floor py) else py
    verb = if walking then "walking" else obj.verb
  in
    {obj| pos=(x, y), verb=verb}

climb: Float -> GameModel -> GameObject -> GameObject
climb delta model obj =
  obj
