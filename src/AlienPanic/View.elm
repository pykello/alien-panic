module AlienPanic.View where

import AlienPanic.Model exposing (..)

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

bgcolor = rgb 174 238 238

view: GameModel -> Element
view model =
  let
    screen  = model.screen
    w = screen.width * screen.unit
    h = screen.height * screen.unit
    objs = List.concat [model.bricks, model.ladders, model.enemies, [model.player]]
  in
    collage (floor w) (floor h)
     (List.concat [
        [rect w h |> filled bgcolor],
        List.map (object_form screen) objs,
        List.map (hole_form screen) model.holes
      ])

object_form: Screen -> GameObject -> Form
object_form screen obj =
  let
    unit = screen.unit
    filename = "images/" ++ obj.name ++
               (if obj.verb == "" then "" else "_" ++ obj.verb) ++
               (if obj.dir == NONE then "" else "_" ++ toString obj.dir) ++
               ".gif"
  in
    image (floor unit) (floor unit) filename
      |> toForm 
      |> move (physical_coord screen (1.0, 1.0) obj.pos)

hole_form: Screen -> Hole -> Form
hole_form screen hole =
  let
    width = hole.width
    depth = hole.depth
    unit = screen.unit
    (x, y) = hole.pos
  in
    rect (width * unit) (depth * unit) |>
    filled bgcolor |>
    move (physical_coord screen (width, depth) (x, y + 1.0 - depth))

physical_coord: Screen -> (Float, Float) -> (Float, Float) -> (Float, Float)
physical_coord screen (w, h) (x, y) =
  let
    unit = screen.unit
    dx = unit * (screen.width - w) * -0.5
    dy = unit * (screen.height - h) * -0.5
  in
    (x * unit + dx, y * unit + dy)
