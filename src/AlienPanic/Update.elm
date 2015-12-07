module AlienPanic.Update where

import AlienPanic.Rect as Rect exposing (..)
import AlienPanic.Model exposing (..)
import Keyboard exposing (..)
import List exposing (..)

eps = 1e-6
type alias Arrows = { x:Int, y:Int }

update: (Float, Arrows, Bool) -> GameModel -> GameModel
update (delta, arrows, space) model =
  if model.won || model.lost then
    model
  else
    model
      |> update_timer delta
      |> update_hit_countdown (delta, space)
      |> update_player (delta, arrows)
      |> update_enemies delta
      |> check_death

update_timer delta model =
  {model| time_cur = model.time_cur + delta}

update_hit_countdown (delta, space) model =
  let
    p_hit_countdown = model.hit_countdown
    n_hit_countdown = if p_hit_countdown > 0.0 then
                        p_hit_countdown - delta
                      else if space && model.player.dir `member` [LEFT, RIGHT] then
                        300.0
                      else
                        0.0
    n_model = if p_hit_countdown > 0.0 && n_hit_countdown <= 0.0 then
                hit_target_piston model
              else
                model
  in
    {n_model| hit_countdown=n_hit_countdown}

hit_target_piston: GameModel -> GameModel
hit_target_piston model =
  case target_piston model.pistons model.player of
    Just piston ->
      hit_piston piston model
    Nothing ->
      model

target_piston: List Rect -> GameObject -> Maybe Rect
target_piston pistons player =
  let
    r = player.rect
    test_point = if player.dir == LEFT then
                   (center_x r - 0.5, bottom_y r)
                 else
                   (center_x r + 0.5, bottom_y r)
  in
    head (filter (contains test_point) pistons)

hit_piston: Rect -> GameModel -> GameModel
hit_piston piston model =
  let
    depth = piston_depth piston
    n_enemies =
      if depth < 0.5 then
        model.enemies
      else
        filter (\e -> not (e.rect `on_piston` piston)) model.enemies
    killed = (length n_enemies) < (length model.enemies)
    update_hit_piston = if killed then restore_piston else press_piston
    update_piston = \p -> if p `on_piston` piston then update_hit_piston p else p
    n_pistons = map update_piston model.pistons
  in
    {model| enemies=n_enemies, pistons=n_pistons}

restore_piston piston =
  Rect.move_y (piston_depth piston) piston

press_piston piston =
  if piston_depth piston < 0.5 - eps then
    Rect.move_y -0.2 piston
  else
    piston

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
    (nx, ny) =
      case find_piston model (px + dx, py, w, h) of
        Just piston ->
          if piston_depth piston > 0.5 then
            center piston
          else
            (px + dx, center_y piston)
        Nothing ->
          (px + dx, toFloat (round py))
    walking = (on_platform model (nx, ny, w, h)) &&
              (abs dx > eps)
  in
    if walking then
      {obj| rect=(nx, ny, w, h), verb="walking",
            dir=if dx > 0 then RIGHT else LEFT}
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
      {obj| rect=(nx, ny, w, h), verb="climbing",
            dir=if dy > 0 then UP else DOWN}
    else
      obj

check_death model =
  let
    r = model.player.rect
    test_points = [(left_x r, center_y r), (right_x r, center_y r)]
    dead = any (contains_any test_points << .rect) model.enemies
    lost = model.lost || dead
  in
    {model| lost=lost}

on_platform: GameModel -> Rect -> Bool
on_platform model (x, y, w, h) =
  if on_ladder model (x, y, w, h) then
    on_platform model (x-1, y, w, h) || on_platform model (x+1, y, w, h)
  else
    find_piston model (x, y, w, h) /= Nothing

on_ladder: GameModel -> Rect -> Bool
on_ladder model rect =
  any (Rect.contains (center rect) << .rect) model.ladders

find_piston: GameModel -> Rect -> Maybe Rect
find_piston model rect =
  let
    overlapping_pistons = filter (on_piston rect) model.pistons
  in
    case overlapping_pistons of
      [] -> Nothing
      rect :: [] -> Just rect
      _ -> Nothing

on_piston: Rect -> Rect -> Bool
on_piston obj_rect piston =
  Rect.contains (center_x obj_rect, bottom_y obj_rect) piston

piston_depth: Rect -> Float
piston_depth (x, y, w, h) =
  toFloat (ceiling y) - y
