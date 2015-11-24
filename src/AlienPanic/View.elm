module AlienPanic.View where

import AlienPanic.Model exposing (..)

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

view: GameModel -> Element
view model =
  let
    screen  = model.screen
    w = screen.width * screen.unit
    h = screen.height * screen.unit
    objs = List.concat [[model.player], model.enemies, model.bricks, model.ladders]
  in
    collage (floor w) (floor h)
     (List.append
       [rect w h |> filled (rgb 174 238 238)]
       (List.map (object_form screen) objs)
     )

object_form: Screen -> GameObject -> Form
object_form screen obj =
  let
    (x, y) = obj.pos
    unit = screen.unit
    dx = screen.unit * (screen.width - 1.0) * -0.5
    dy = screen.unit * (screen.height - 1.0) * -0.5
    filename = "images/" ++ obj.name ++
               (if obj.verb == "" then "" else "_" ++ obj.verb) ++
               (if obj.dir == NONE then "" else "_" ++ toString obj.dir) ++
               ".gif"
  in
    image (floor unit) (floor unit) filename
      |> toForm 
      |> move (x * unit + dx, y * unit + dy)
