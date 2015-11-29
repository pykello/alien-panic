module AlienPanic.Update where

import AlienPanic.Model exposing (..)
import Keyboard exposing (..)
import List exposing (member, map)

eps = 1e-6
type alias Keys = { x:Int, y:Int }

update: (Float, Keys) -> GameModel -> GameModel
update (delta, keys) model =
  model
    |> update_player (delta, keys)
    |> update_enemies delta

update_player: (Float, Keys) -> GameModel -> GameModel
update_player (delta, keys) model =
  let
    dx = 0.00075 * delta * (toFloat keys.x)
    dy = 0.00075 * delta * (toFloat keys.y)
    player = model.player |> reset |> walk dx model |> climb dy model
  in
    {model| player=player}

update_enemies: Float -> GameModel -> GameModel
update_enemies delta model =
  let
    enemies = map (update_enemy delta model) model.enemies
  in
    {model| enemies=enemies}

update_enemy: Float -> GameModel -> GameObject -> GameObject
update_enemy delta model enemy =
  let
    (px, py) = enemy.pos
    dx = 0.00045 * delta * (if enemy.dir == LEFT then -1.0 else 1.0)
    (nx, ny) = (px + dx, py)
    can_move = (on_platform model (nx, ny))
  in
    if can_move then
      {enemy| pos=(nx, ny), verb="walking"}
    else if enemy.dir == LEFT then
      {enemy| dir=RIGHT, verb=""}
    else
      {enemy| dir=LEFT, verb=""}

reset: GameObject -> GameObject
reset obj =
  {obj| verb=""}

walk: Float -> GameModel -> GameObject -> GameObject
walk dx model obj =
  let
    (px, py) = obj.pos
    (nx, ny) = (px + dx, toFloat (round py))
    walking = (on_platform model (nx, ny)) &&
              (abs dx > eps)
  in
    if walking then
      {obj| pos=(nx, ny), verb="walking",
            dir=if dx > 0 then RIGHT else LEFT}
    else
      obj

climb: Float -> GameModel -> GameObject -> GameObject
climb dy model obj =
  let
    (px, py) = obj.pos
    (nx, ny) = (toFloat (round px), py + dy)
    climbing = (on_ladder model (nx, ny)) &&
               (abs dy > eps)
  in
    if climbing then
      {obj| pos=(nx, ny), verb="climbing",
            dir=if dy > 0 then UP else DOWN}
    else
      obj

on_platform: GameModel -> (Float, Float) -> Bool
on_platform model pos =
  let
    platforms = map (\o -> grid_pos o.pos) model.bricks
    ladders = map (\o -> grid_pos o.pos) model.ladders
    (x, y) = grid_pos pos
  in
    (x,y-1) `member` platforms ||
    (x,y-1) `member` ladders &&
      ((x-1,y-1) `member` platforms || (x+1,y-1) `member` platforms)

on_ladder: GameModel -> (Float, Float) -> Bool
on_ladder model pos =
  let
    ladders = (map (\o -> grid_pos o.pos) model.ladders)
  in
    (grid_pos pos) `member` ladders

grid_pos: (Float, Float) -> (Int, Int)
grid_pos (x, y) =
  (round x, floor y)
