module AlienPanic.Model where

import AlienPanic.Rect as Rect exposing (..)
import String exposing (indexes, length)
import List exposing (map, head, length)

type Dir = LEFT | RIGHT | UP | DOWN | NONE

type alias GameObject = {
  rect: Rect,
  dir: Dir,
  name: String,
  verb: String
}

type alias GameModel = {
  player: GameObject,
  enemies: List GameObject,
  bricks: List GameObject,
  ladders: List GameObject,
  pistons: List Rect,
  time_max: Int,
  time_cur: Int,
  hit_countdown: Float,
  won: Bool,
  lost: Bool,
  size: (Int, Int)
}

from_tiles: Int -> List String -> Maybe GameModel
from_tiles duration tiles =
  let
    bricks = search_grid '#' tiles |> map (\p ->
                {rect=from_pos p 1.0 1.0, dir=NONE, name="brick", verb=""})
    pistons = List.concat (map (platform_pistons << .rect) bricks)
    size = case head tiles of
             Nothing -> (0, 0)
             Just row -> (String.length row, List.length tiles)
  in
    case search_grid 'P' tiles of
      [] -> Nothing
      p :: [] -> Just {
                 player = {rect=from_pos p 0.35 1.0,
                           dir=RIGHT, name="player", verb=""},
                 enemies = search_grid 'E' tiles |> map (\p ->
                              {rect=from_pos p 0.55 1.0,
                               dir=LEFT, name="enemy", verb=""}),
                 bricks = bricks,
                 ladders = search_grid '|' tiles |> map (\p ->
                              {rect=from_pos p 1.0 1.0,
                               dir=NONE, name="ladder", verb=""}),
                 pistons = pistons,
                 hit_countdown = 0,
                 time_cur = 0,
                 time_max = duration * 1000,
                 won = False,
                 lost = False,
                 size = size
               }
      xs -> Nothing

platform_pistons: Rect -> List Rect
platform_pistons (x, y, w, h) =
  [
    (x - w/4.0, y + h, w * 0.5, h),
    (x + w/4.0, y + h, w * 0.5, h)
  ]

search_grid: Char -> List String -> List (Float, Float)
search_grid ch board =
  List.reverse board |>
  List.indexedMap (search_row ch) |>
  List.concat
  
search_row: Char -> Int -> String -> List (Float, Float)
search_row ch y row =
  row |>
  String.indexes (String.fromChar ch) |>
  List.map (\x -> (toFloat x, toFloat y))
