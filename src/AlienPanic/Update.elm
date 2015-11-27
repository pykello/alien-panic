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
    |> climb delta keys model

update_dir: Keys -> GameModel -> GameObject -> GameObject
update_dir keys model obj =
  {obj| dir = case (keys.x, keys.y) of
                (-1, _) -> if on_platform model obj.pos then LEFT else obj.dir
                (1, _)  -> if on_platform model obj.pos then RIGHT else obj.dir
                (_ , 1) -> if on_ladder model obj.pos then UP else obj.dir
                (_, -1) -> if on_ladder model obj.pos then DOWN else obj.dir
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
    y = if walking then toFloat (floor (py + 0.5001)) else py
    verb = if walking then "walking" else obj.verb
  in
    {obj| pos=(x, y), verb=verb}

climb: Float -> Keys -> GameModel -> GameObject -> GameObject
climb delta keys model obj =
  let
    (px, py) = obj.pos
    dy = 0.00075 * delta * (toFloat keys.y)
    (nx, ny) = (toFloat (floor (px + 0.5001)), py + dy)
    climbing = (obj.dir == UP || obj.dir == DOWN) &&
               (on_ladder model (nx, ny))
    (x, y) = if climbing then (nx, ny) else (px, py)
    verb = if climbing then "climbing" else obj.verb
  in
    {obj| pos=(x, y), verb=verb}

on_platform: GameModel -> (Float, Float) -> Bool
on_platform model pos =
  let
    platforms = (List.map (\o -> grid_pos o.pos) model.bricks)
    (x, y) = grid_pos pos
  in
    (List.member (x, y - 1) platforms) ||
    (List.member (x - 1, y - 1) platforms) ||
    (List.member (x + 1, y - 1) platforms)

on_ladder: GameModel -> (Float, Float) -> Bool
on_ladder model pos =
  List.member (grid_pos pos)
              (List.map (\o -> grid_pos o.pos) model.ladders)


grid_pos: (Float, Float) -> (Int, Int)
grid_pos (x, y) =
  (floor (x + 0.5001), floor (y + 0.5001))
