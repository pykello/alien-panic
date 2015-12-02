module AlienPanic.View where

import AlienPanic.Model exposing (..)
import AlienPanic.Rect exposing (..)

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (concat, map)

bgcolor = rgb 174 238 238
debug = True

view: GameModel -> Element
view model =
  let
    screen  = model.screen
    w = screen.width * screen.unit
    h = screen.height * screen.unit
  in
    collage (floor w) (floor h)
     (concat [
        [rect w h |> filled bgcolor],
        map (object_form screen) (model.bricks ++ model.ladders),
        map (hole_form screen) model.holes,
        if debug then [rect_form screen (rgb 0 0 255) model.player.rect] else [],
        map (object_form screen) (model.player :: model.enemies)
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
      |> move (physical_coord screen obj.rect)

hole_form: Screen -> Rect -> Form
hole_form screen rect =
  let
    (x, _, _, _) = rect
    color = if not debug then
              bgcolor
            else if (x - toFloat (floor x)) > 0.5 then
              (rgb 255 0 0)
            else
              (rgb 0 255 0)
  in
    rect_form screen color rect

rect_form: Screen -> Color -> Rect -> Form
rect_form screen color (x, y, w, h) =
  rect (w * screen.unit) (h * screen.unit) |>
  filled color |>
  move (physical_coord screen (x, y, w, h))

physical_coord: Screen -> Rect -> (Float, Float)
physical_coord screen (x, y, w, h) =
  let
    unit = screen.unit
    dx = unit * (screen.width - w) * -0.5
    dy = unit * (screen.height - h) * -0.5
  in
    (x * unit + dx, y * unit + dy)
