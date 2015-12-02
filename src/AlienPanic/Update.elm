module AlienPanic.Update where

import AlienPanic.Rect as Rect exposing (..)
import AlienPanic.Model exposing (..)
import Keyboard exposing (..)
import List exposing (..)

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
              dig_hole model.holes model.player
            else
              model.holes
  in
    {model| hit_countdown=n_hit_countdown, holes=holes}

dig_hole holes player =
  let
    r = player.rect
    test_point = if player.dir == LEFT then
                   (left_x r, bottom_y r)
                 else
                   (right_x r, bottom_y r)
  in
    holes |>
      map (\hole -> if contains test_point hole then
                     hole
                    else
                     hole)

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
    (px, py, w, h) = obj.rect
    (nx, ny) = (px + dx, toFloat (round py))
    depth = hole_depth model (nx, ny, w, h)
    walking = (on_platform model (nx, ny, w, h)) &&
              (abs dx > eps)
  in
    if walking then
      {obj| pos=(nx, ny-depth), rect=(nx, ny-depth, w, h),
            verb="walking", dir=if dx > 0 then RIGHT else LEFT}
    else
      obj

climb: Float -> GameModel -> GameObject -> GameObject
climb dy model obj =
  let
    (px, py, w, h) = obj.rect
    (nx, ny) = (toFloat (round px), py + dy)
    climbing = (on_ladder model (nx, ny, w, h)) &&
               (abs dy > eps)
  in
    if climbing then
      {obj| pos=(nx, ny), rect=(nx, ny, w, h),
            verb="climbing", dir=if dy > 0 then UP else DOWN}
    else
      obj

on_platform: GameModel -> Rect -> Bool
on_platform model rect =
  let
    platforms_beneath = filter (Rect.is_under rect << .rect) model.bricks
  in
    not (List.isEmpty platforms_beneath)

on_ladder: GameModel -> Rect -> Bool
on_ladder model rect =
  let
    c = center rect
    overlapping_ladders = filter (Rect.contains c << .rect) model.ladders 
  in
    not (List.isEmpty overlapping_ladders)

hole_depth: GameModel -> Rect -> Float
hole_depth model rect =
  let
    p = (center_x rect, bottom_y rect)
    hole_below = head (filter (Rect.contains p) model.holes)
  in
    case hole_below of
      Just (x, y, w, h) -> toFloat (ceiling y) - y
      Nothing -> 0.0
