module AlienPanic.Update where

import AlienPanic.Model exposing (..)
import Keyboard exposing (..)
import List exposing (member, map)

eps = 1e-6
type alias Keys = { x:Int, y:Int }

update: (Float, Keys) -> GameModel -> GameModel
update (delta, keys) model =
  {model|
    player=update_player (delta, keys) model model.player}

update_player: (Float, Keys) -> GameModel -> GameObject -> GameObject
update_player (delta, keys) model player =
  let
    dx = 0.00075 * delta * (toFloat keys.x)
    dy = 0.00075 * delta * (toFloat keys.y)
  in
    {player| verb=""}
      |> walk dx model
      |> climb dy model

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
