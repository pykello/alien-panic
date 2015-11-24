module AlienPanic.View where

import AlienPanic.Model exposing (..)

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

view: GameModel -> Element
view model =
  let
    screen  = model.screen
    objs = List.concat [[model.player], model.enemies, model.bricks, model.ladders]
  in
    collage
     (floor screen.width) (floor screen.height)
     (List.append
       [background_form screen]
       (List.map (object_form screen) objs)
     )
      
background_form: Screen -> Form
background_form screen =
  rect screen.width screen.height
    |> filled (rgb 174 238 238)
    
object_form: Screen -> GameObject -> Form
object_form screen obj =
  let
    (x, y) = obj.pos
    unit = screen.unit
    dx = (screen.width - unit) * -0.5
    dy = (screen.height - unit) * -0.5
    filename = "images/" ++ obj.name ++
               (if obj.verb == "" then "" else "_" ++ obj.verb) ++
               (if obj.dir == NONE then "" else "_" ++ toString obj.dir) ++
               ".gif"
  in
    image (floor unit) (floor unit) filename
      |> toForm 
      |> move (x * unit + dx, y * unit + dy)
