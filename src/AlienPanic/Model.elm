module AlienPanic.Model where

import String exposing (indexes)

type Dir = LEFT | RIGHT | UP | DOWN | NONE

type alias Creature = {
  pos: (Float, Float),
  dir: Dir,
  name: String
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
  case get_board_tiles "player" 'P' tiles of
    [] -> Nothing
    p :: [] -> Just {
                 screen = screen_from_tiles unit tiles,
                 player = p,
                 enemies = get_board_tiles "enemy" 'E' tiles,
                 bricks = get_board_tiles "brick" '#' tiles,
                 ladders = get_board_tiles "ladder" '|' tiles
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
init = from_tiles 32 [".........",
                      "......|..",
                      "E.|...|.E",
                      "##|###|##",
                      "..|...|..",
                      "..|...|..",
                      "##|######",
                      "..|......",
                      "P.|....E.",
                      "#########"]

get_board_tiles: String -> Char -> List String -> List Creature
get_board_tiles name ch board =
  List.reverse board |>
  List.indexedMap (\y row -> get_row_tiles name ch y row) |>
  List.concat
  
get_row_tiles: String -> Char -> Int -> String -> List Creature
get_row_tiles name ch y row =
  row |>
  String.indexes (String.fromChar ch) |>
  List.map (\x -> {pos = (toFloat x, toFloat y), dir = NONE, name = name})
