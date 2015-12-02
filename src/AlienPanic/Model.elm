module AlienPanic.Model where

import AlienPanic.Rect as Rect exposing (..)
import String exposing (indexes)
import List exposing (map)

type Dir = LEFT | RIGHT | UP | DOWN | NONE
type alias Pos = (Float, Float)

type alias GameObject = {
  pos: Pos,
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
  holes: List Rect,
  hit_countdown: Float
}

from_tiles: Int -> List String -> Maybe GameModel
from_tiles unit tiles =
  let
    bricks = search_grid '#' tiles |> map (\p ->
                {pos=p, rect=from_pos p 1.0 1.0, dir=NONE, name="brick", verb=""})
    holes = List.concat (map (platform_holes << .rect) bricks)
  in
    case search_grid 'P' tiles of
      [] -> Nothing
      p :: [] -> Just {
                 screen = screen_from_tiles unit tiles,
                 player = {pos=p, rect=from_pos p 1.0 1.0,
                           dir=RIGHT, name="player", verb=""},
                 enemies = search_grid 'E' tiles |> map (\p ->
                              {pos=p, rect=from_pos p 1.0 1.0,
                               dir=LEFT, name="enemy", verb=""}),
                 bricks = bricks,
                 ladders = search_grid '|' tiles |> map (\p ->
                              {pos=p, rect=from_pos p 1.0 1.0,
                               dir=NONE, name="ladder", verb=""}),
                 holes = holes,
                 hit_countdown = 0
               }
      xs -> Nothing

platform_holes: Rect -> List Rect
platform_holes (x, y, w, h) =
  [
    (x - w/4.0, y + h - 0.2, w * 0.5, h),
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

search_grid: Char -> List String -> List Pos
search_grid ch board =
  List.reverse board |>
  List.indexedMap (search_row ch) |>
  List.concat
  
search_row: Char -> Int -> String -> List Pos
search_row ch y row =
  row |>
  String.indexes (String.fromChar ch) |>
  List.map (\x -> (toFloat x, toFloat y))
