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

left_x (x, _, w, _) =
  x - w * 0.5

right_x (x, _, w, _) =
  x + w * 0.5

is_under: Rect -> Rect -> Bool
is_under rect1 rect2 =
  range_overlaps (x_range rect2) (x_range rect1) &&
  range_overlaps (y_range_top rect2) (bottom rect1, bottom rect1)

overlaps rect1 rect2 =
  range_overlaps (x_range rect1) (x_range rect2) &&
  range_overlaps (y_range rect1) (y_range rect2)

contains: (Float, Float) -> Rect -> Bool
contains (x1, y1) rect2 =
  range_overlaps (x_range rect2) (x1, x1) &&
  range_overlaps (y_range rect2) (y1, y1)

x_range (x, _, w, _) =
  (x - w * 0.5, x + w * 0.5)

y_range (_, y, _, h) =
  (y - h * 0.5, y + h * 0.5)

y_range_top (_, y, _, h) =
  (y, y + h * 0.5)

top_half (x, y, w, h) =
  (x, y+h*0.25, w, h*0.5)

bottom rect =
  fst (y_range rect)

range_overlaps (a, b) (c, d) =
  (max a c) < (min b d) + eps
