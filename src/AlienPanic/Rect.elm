module AlienPanic.Rect where

type alias Rect = (Float, Float, Float, Float)
eps = 1e-6

from_pos: (Float, Float) -> Float -> Float -> Rect
from_pos (x, y) w h =
  (x, y, w, h)

center_x (x, _, _, _) =
  x

center_y (_, y, _, _) =
  y

center rect =
  (center_x rect, center_y rect)

bottom_y (_, y, _, h) =
  y - h * 0.5

top_y (_, y, _, h) =
  y + h * 0.5

left_x (x, _, w, _) =
  x - w * 0.5

right_x (x, _, w, _) =
  x + w * 0.5

overlaps rect1 rect2 =
  range_overlaps (x_range rect1) (x_range rect2) &&
  range_overlaps (y_range rect1) (y_range rect2)

contains: (Float, Float) -> Rect -> Bool
contains (x, y) rect =
  x > (left_x rect) - eps && x < (right_x rect) - eps &&
  y > (bottom_y rect) - eps && y < (top_y rect) - eps

contains_any: List (Float, Float) -> Rect -> Bool
contains_any points rect =
  List.any (\p -> contains p rect) points

x_range (x, _, w, _) =
  (x - w * 0.5, x + w * 0.5)

y_range (_, y, _, h) =
  (y - h * 0.5, y + h * 0.5)

range_overlaps (a, b) (c, d) =
  (max a c) < (min b d) + eps

move_x dx (x, y, w, h) =
  (x+dx, y, w, h)

move_y dy (x, y, w, h) =
  (x, y+dy, w, h)
