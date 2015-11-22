import Color exposing (..)
import String exposing (indexes)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

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
  tiles: List String,
  player: Creature,
  enemies: List Creature
}

init: GameModel
init =
 {
   screen = {unit = 30.0, width = 270.0, height = 300.0},
   tiles = [".........",
            ".........",
            "......|..",
            "######|##",
            "......|..",
            "......|..",
            "##|######",
            "..|......",
            "..|......",
            "#########"],
    player = {pos = (0.0, 1.0), dir = RIGHT},
    enemies = [
                {pos = (1.0, 7.0), dir = RIGHT},
                {pos = (8.0, 7.0), dir = LEFT}
              ]
 }

view: GameModel -> Element
view model =
  let
    screen  = model.screen
  in
    collage
     (floor screen.width) (floor screen.height)
     (List.append
       [background screen]
       (get_creatures model |> List.map (creature_form screen))
     )
      
background: Screen -> Form
background screen =
  rect screen.width screen.height
    |> filled (rgb 174 238 238)
    
creature_form: Screen -> Creature -> Form
creature_form screen creature =
  let
    (x, y) = creature.pos
    unit = screen.unit
    dx = (screen.width - unit) * -0.5
    dy = (screen.height - unit) * -0.5
  in
    rect unit unit
      |> filled (rgb 255 0 0)
      |> move (x * unit + dx, y * unit + dy)

get_creatures: GameModel -> List Creature
get_creatures model =
  List.concat [
    [model.player],
    model.enemies,
    get_board_tiles '#' model.tiles,
    get_board_tiles '|' model.tiles]

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

main =
  view init
