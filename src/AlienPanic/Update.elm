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

{-| Depending on keys.x/y, player tries to walk or climb. -}
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

{-| Enemies walk in the current direction if they can, otherwise
    they stop. When they stop, they change direction. -}
update_enemy: Float -> GameModel -> GameObject -> GameObject
update_enemy delta model enemy =
  let
    dx = 0.00045 * delta * (if enemy.dir == LEFT then -1.0 else 1.0)
  in
    enemy |> reset |> walk dx model |> update_enemy_dir

update_enemy_dir: GameObject -> GameObject
update_enemy_dir enemy =
  {enemy| dir = case (enemy.verb, enemy.dir) of
                  ("", LEFT) -> RIGHT
                  ("", RIGHT) -> LEFT
                  (_, _) -> enemy.dir}

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
    platforms = map (grid_pos << .pos) model.bricks
    ladders = map (grid_pos << .pos) model.ladders
    (x, y) = grid_pos pos
  in
    (x,y-1) `member` platforms ||
    (x,y-1) `member` ladders &&
      ((x-1,y-1) `member` platforms || (x+1,y-1) `member` platforms)

on_ladder: GameModel -> (Float, Float) -> Bool
on_ladder model pos =
  let
    ladders = (map (grid_pos << .pos) model.ladders)
  in
    (grid_pos pos) `member` ladders

grid_pos: (Float, Float) -> (Int, Int)
grid_pos (x, y) =
  (round x, floor y)
