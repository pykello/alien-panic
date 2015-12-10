module AlienPanic.View where

import AlienPanic.Model exposing (..)
import AlienPanic.Rect exposing (..)

import Color exposing (..)
import Graphics.Collage as Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (..)
import List exposing (concat, map)

bgcolor = rgb 174 238 238
unit = 64
screen_w = 9 * 64
screen_h = 11 * 64

view: GameModel -> Element
view model =
  flow down [
    view_board model,
    view_timer model
  ]

view_timer model =
  let
    timer_w = screen_w
    timer_h = unit // 2
    passed_w = (timer_w * model.time_cur) // model.time_max
  in
    layers [rect_elem timer_w timer_h (rgb 100 100 100),
            rect_elem passed_w timer_h (rgb 100 0 0)]

rect_elem rect_w rect_h color =
  collage rect_w rect_h [
    rect (toFloat rect_w) (toFloat rect_h) |> filled color
  ]

rect_i w h =
  rect (toFloat w) (toFloat h)

view_board: GameModel -> Element
view_board model =
  collage screen_w screen_h
   [List.concat [
      [rect_i screen_w screen_h |> filled bgcolor |>
        move (screen_w * 0.5, screen_h * 0.5)],
      map object_form model.bricks,
      map piston_form model.pistons,
      map object_form model.ladders,
      map object_form (model.player :: model.enemies)
    ] |> group |> move (-screen_w * 0.5, -screen_h * 0.5)]

object_form: GameObject -> Form
object_form obj =
  let
    (x, y, w, h) = obj.rect
    filename = "images/" ++ obj.name ++
               (if obj.verb == "" then "" else "_" ++ obj.verb) ++
               (if obj.dir == NONE then "" else "_" ++ toString obj.dir) ++
               ".gif"
  in
    image unit unit filename |> toForm 
      |> move ((x + 0.5) * unit + w * 0.5, (y + 0.5) * unit + h * 0.5)

piston_form: Rect -> Form
piston_form (x, y, w, h) =
  rect (w * toFloat unit) (h * toFloat unit) |>
  filled bgcolor |>
  move ((x + 0.5) * unit + w * 0.5, (y + 0.5) * unit + h * 0.5)
