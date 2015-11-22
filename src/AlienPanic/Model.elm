module AlienPanic.Model where

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
