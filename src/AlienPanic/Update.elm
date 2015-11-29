module AlienPanic.Update where

import AlienPanic.Model exposing (..)
import Keyboard exposing (..)
import List exposing (member)

type alias Keys = { x:Int, y:Int }

update: (Float, Keys) -> GameModel -> GameModel
update (delta, keys) model =
  {model|
    player=update_player (delta, keys) model model.player}

update_player: (Float, Keys) -> GameModel -> GameObject -> GameObject
update_player (delta, keys) model player =
  {player| verb=""}
    |> walk delta keys model
    |> climb delta keys model

walk: Float -> Keys -> GameModel -> GameObject -> GameObject
walk delta keys model obj =
  let
    (px, py) = obj.pos
    (nx, ny) = (px + 0.00075 * delta * (toFloat keys.x),
                toFloat (round py))
    walking = (on_platform model (nx, ny)) &&
              (keys.x /= 0)
  in
    if walking then
      {obj| pos=(nx, ny), verb="walking",
            dir=if keys.x > 0 then RIGHT else LEFT}
    else
      obj

climb: Float -> Keys -> GameModel -> GameObject -> GameObject
climb delta keys model obj =
  let
    (px, py) = obj.pos
    (nx, ny) = (toFloat (round px),
                py + 0.00075 * delta * (toFloat keys.y))
    climbing = (on_ladder model (nx, ny)) &&
               (keys.y /= 0)
  in
    if climbing then
      {obj| pos=(nx, ny), verb="climbing",
            dir=if keys.y > 0 then UP else LEFT}
    else
      obj

on_platform: GameModel -> (Float, Float) -> Bool
on_platform model pos =
  let
    platforms = List.map (\o -> grid_pos o.pos) model.bricks
    ladders = List.map (\o -> grid_pos o.pos) model.ladders
    (x, y) = grid_pos pos
  in
    (x,y-1) `member` platforms ||
    (x,y-1) `member` ladders &&
      ((x-1,y-1) `member` platforms || (x+1,y-1) `member` platforms)

on_ladder: GameModel -> (Float, Float) -> Bool
on_ladder model pos =
  member (grid_pos pos)
         (List.map (\o -> grid_pos o.pos) model.ladders)

grid_pos: (Float, Float) -> (Int, Int)
grid_pos (x, y) =
  (round x, floor y)
