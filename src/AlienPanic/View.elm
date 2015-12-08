module AlienPanic.View where

import AlienPanic.Model exposing (..)
import AlienPanic.Rect exposing (..)

import Color exposing (..)
import Graphics.Collage as Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)
import List exposing (concat, map)

bgcolor = rgb 174 238 238
message_style = {defaultStyle|
                  color=white,
                  height=Just 20,
                  typeface=["arial","sans-serif"]}
debug = False

view: GameModel -> Element
view model =
  flow down [
    view_board model,
    view_timer model
  ]

view_timer: GameModel -> Element
view_timer model =
  let
    screen = model.screen
    w = screen.width * screen.unit
    h = screen.unit * 0.5
    time_ratio = model.time_cur / model.time_max
  in
    collage (floor w) (floor h)
    [
      rect w h |>
        filled (rgb 100 100 100),
      rect (w * time_ratio) h |>
        filled (rgb 100 0 0) |>
        Collage.moveX (w * time_ratio / 2.0 - w / 2.0)
    ]

view_board: GameModel -> Element
view_board model =
  let
    screen  = model.screen
    w = screen.width * screen.unit
    h = screen.height * screen.unit
    (px, py, pw, ph) = model.player.rect
  in
    collage (floor w) (floor h)
     (List.concat [
        [rect w h |> filled bgcolor],
        map (object_form screen) model.bricks,
        map (piston_form screen) model.pistons,
        if debug then 
          [
            rect_form screen (rgb 0 0 255) (px, py, 0.05, 2),
            rect_form screen (rgb 0 0 255) (px, py, 2, 0.05)
          ] 
        else [],
        map (object_form screen) model.ladders,
        map (object_form screen) (model.player :: model.enemies)
      ])

object_form: Screen -> GameObject -> Form
object_form screen obj =
  let
    unit = screen.unit
    (x, y, w, h) = obj.rect
    filename = "images/" ++ obj.name ++
               (if obj.verb == "" then "" else "_" ++ obj.verb) ++
               (if obj.dir == NONE then "" else "_" ++ toString obj.dir) ++
               ".gif"
    debug_rect = rect (w * screen.unit) (h * screen.unit) |>
                 outlined defaultLine
  in
    group (
      (if debug then [debug_rect] else []) ++
      [image (floor unit) (floor unit) filename |> toForm]
    ) |> Collage.move (physical_coord screen obj.rect)

piston_form: Screen -> Rect -> Form
piston_form screen rect =
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
  Collage.move (physical_coord screen (x, y, w, h))

physical_coord: Screen -> Rect -> (Float, Float)
physical_coord screen (x, y, w, h) =
  let
    unit = screen.unit
    dx = unit * (screen.width * -0.5 + 0.5)
    dy = unit * (screen.height * -0.5 + 0.5)
  in
    (x * unit + dx, y * unit + dy)
