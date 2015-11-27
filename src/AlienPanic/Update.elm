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
    |> update_dir keys model
    |> walk delta keys model
    |> climb delta model

update_dir: Keys -> GameModel -> GameObject -> GameObject
update_dir keys model obj =
  {obj| dir = case (keys.x, keys.y) of
                (-1, _) -> if on_platform model obj then LEFT else obj.dir
                (1, _)  -> if on_platform model obj then RIGHT else obj.dir
                (_ , 1) -> if on_ladder model obj then UP else obj.dir
                (_, -1) -> if on_ladder model obj then DOWN else obj.dir
                (_, _)  -> obj.dir}

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

on_platform model obj =
  True

on_ladder model obj =
  List.member (grid_pos obj)
              (List.map grid_pos model.ladders)


grid_pos: GameObject -> (Int, Int)
grid_pos obj =
  let
    (x, y) = obj.pos
  in
    (floor (x + 0.5001), floor y)
