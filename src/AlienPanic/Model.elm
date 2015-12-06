module AlienPanic.Model where

import AlienPanic.Rect as Rect exposing (..)
import String exposing (indexes)
import List exposing (map)

type Dir = LEFT | RIGHT | UP | DOWN | NONE

type alias GameObject = {
  rect: Rect,
  dir: Dir,
  name: String,
  verb: String
}

type alias Screen = {
  unit: Float,
  width: Float,
  height: Float
}

type alias GameModel = {
  screen: Screen,
  player: GameObject,
  enemies: List GameObject,
  bricks: List GameObject,
  ladders: List GameObject,
  pistons: List Rect,
  hit_countdown: Float,
  pending_hit: Bool,
  won: Bool,
  lost: Bool
}

from_tiles: Int -> List String -> Maybe GameModel
from_tiles unit tiles =
  let
    bricks = search_grid '#' tiles |> map (\p ->
                {rect=from_pos p 1.0 1.0, dir=NONE, name="brick", verb=""})
    pistons = List.concat (map (platform_pistons << .rect) bricks)
  in
    case search_grid 'P' tiles of
      [] -> Nothing
      p :: [] -> Just {
                 screen = screen_from_tiles unit tiles,
                 player = {rect=from_pos p 0.5 1.0,
                           dir=RIGHT, name="player", verb=""},
                 enemies = search_grid 'E' tiles |> map (\p ->
                              {rect=from_pos p 0.5 1.0,
                               dir=LEFT, name="enemy", verb=""}),
                 bricks = bricks,
                 ladders = search_grid '|' tiles |> map (\p ->
                              {rect=from_pos p 1.0 1.0,
                               dir=NONE, name="ladder", verb=""}),
                 pistons = pistons,
                 hit_countdown = 0,
                 pending_hit = False,
                 won = False,
                 lost = False
               }
      xs -> Nothing

platform_pistons: Rect -> List Rect
platform_pistons (x, y, w, h) =
  [
    (x - w/4.0, y + h, w * 0.5, h),
    (x + w/4.0, y + h, w * 0.5, h)
  ]

screen_from_tiles: Int -> List String -> Screen
screen_from_tiles unit tiles =
  case tiles of
    [] -> {unit = 0.0, width = 0.0, height = 0.0}
    x :: xs -> {
                 unit = toFloat unit,
                 width = toFloat (String.length(x)),
                 height = toFloat (List.length(tiles))
               }

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
