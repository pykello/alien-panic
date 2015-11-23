module AlienPanic.Model where

import String exposing (indexes)

type Dir = LEFT | RIGHT | UP | DOWN | NONE

type alias Creature = {
  pos: (Float, Float),
  dir: Dir 
}

type alias Screen = {
  unit: Float,
  width: Float,
  height: Float
}

type alias GameModel = {
  screen: Screen,
  player: Creature,
  enemies: List Creature,
  bricks: List Creature,
  ladders: List Creature
}

from_tiles: Int -> List String -> Maybe GameModel
from_tiles unit tiles =
  case get_board_tiles 'P' tiles of
    [] -> Nothing
    p :: [] -> Just {
                 screen = screen_from_tiles unit tiles,
                 player = p,
                 enemies = get_board_tiles 'E' tiles,
                 bricks = get_board_tiles '#' tiles,
                 ladders = get_board_tiles '|' tiles
               }
    xs -> Nothing

screen_from_tiles: Int -> List String -> Screen
screen_from_tiles unit tiles =
  case tiles of
    [] -> {unit = 0.0, width = 0.0, height = 0.0}
    x :: xs -> {
                 unit = toFloat unit,
                 width = toFloat (String.length(x) * unit),
                 height = toFloat (List.length(tiles) * unit)
               }

init: Maybe GameModel
init = from_tiles 30 [".........",
                      ".........",
                      "E.....|.E",
                      "######|##",
                      "......|..",
                      "......|..",
                      "##|######",
                      "..|......",
                      "P.|......",
                      "#########"]

get_board_tiles: Char -> List String -> List Creature
get_board_tiles ch board =
  List.reverse board |>
  List.indexedMap (\y row -> get_row_tiles ch y row) |>
  List.concat
  
get_row_tiles: Char -> Int -> String -> List Creature
get_row_tiles ch y row =
  row |>
  String.indexes (String.fromChar ch) |>
  List.map (\x -> {pos = (toFloat x, toFloat y), dir = NONE})
