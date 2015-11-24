module AlienPanic.Model where

import String exposing (indexes)
import List exposing (map)

type Dir = LEFT | RIGHT | UP | DOWN | NONE
type alias Pos = (Float, Float)

type alias GameObject = {
  pos: Pos,
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
  ladders: List GameObject
}

from_tiles: Int -> List String -> Maybe GameModel
from_tiles unit tiles =
  case search_grid 'P' tiles of
    [] -> Nothing
    p :: [] -> Just {
                 screen = screen_from_tiles unit tiles,
                 player = {pos=p, dir=RIGHT, name="player", verb=""},
                 enemies = search_grid 'E' tiles |> map (\p ->
                              {pos=p, dir=NONE, name="enemy", verb=""}),
                 bricks = search_grid '#' tiles |> map (\p ->
                              {pos=p, dir=NONE, name="brick", verb=""}),
                 ladders = search_grid '|' tiles |> map (\p ->
                              {pos=p, dir=NONE, name="ladder", verb=""})
               }
    xs -> Nothing

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
  List.indexedMap (\y row -> search_row ch y row) |>
  List.concat
  
search_row: Char -> Int -> String -> List Pos
search_row ch y row =
  row |>
  String.indexes (String.fromChar ch) |>
  List.map (\x -> (toFloat x, toFloat y))
