module AlienPanic.Update where

import AlienPanic.Model exposing (..)
import Keyboard exposing (..)
import List exposing (member, map)

eps = 1e-6
type alias Arrows = { x:Int, y:Int }

update: (Float, Arrows, Bool) -> GameModel -> GameModel
update (delta, arrows, space) model =
  model
    |> update_hits (delta, space)
    |> update_player (delta, arrows)
    |> update_enemies delta

update_hits (delta, space) model =
  let
    p_hit_countdown = model.hit_countdown
    n_hit_countdown = if p_hit_countdown > 0.0 then
                        p_hit_countdown - delta
                      else if space then
                        300.0
                      else
                        0.0
    holes = if p_hit_countdown > 0.0 && n_hit_countdown <= 0.0 then
              dig_hole model.holes (hole_coord model.player)
            else
              model.holes
  in
    {model| hit_countdown=n_hit_countdown, holes=holes}

hole_coord player =
  let
    (player_x, player_y) = player.pos
    dx = if player.dir == RIGHT then 1.0 else -0.0
    hole_x = toFloat (floor ((player_x + dx) * 2.0)) * 0.5
    hole_y = player_y - 1.0
  in
    (hole_x, hole_y)

dig_hole holes pos =
  holes |>
    map (\hole -> if (pos_eq hole.pos pos) then
                   {hole|depth=min (hole.depth+0.2) 1.0}
                 else
                   hole)

pos_eq (x1, y1) (x2, y2) =
  abs (x1 - x2) < eps &&
  abs (y1 - y2) < eps

update_player: (Float, Arrows) -> GameModel -> GameModel
update_player (delta, arrows) model =
  let
    pplayer = model.player
    nplayer = if model.hit_countdown > 0.0 then
                {pplayer| verb="hitting"}
              else
                move_player (delta, arrows) model
  in
    {model| player=nplayer}

{-| Depending on arrows.x/y, player tries to walk or climb. -}
move_player: (Float, Arrows) -> GameModel -> GameObject
move_player (delta, arrows) model =
  let
    dx = 0.00075 * delta * (toFloat arrows.x)
    dy = 0.00075 * delta * (toFloat arrows.y)
  in
    model.player |> reset |> walk dx model |> climb dy model

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
