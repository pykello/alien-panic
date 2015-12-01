module AlienPanic.Rect where

type alias Rect = (Float, Float, Float, Float)
eps = 1e-6

from_pos: (Float, Float) -> Float -> Float -> Rect
from_pos (x, y) w h =
  (x, y, w, h)

on_top: Rect -> Rect -> Bool
on_top rect1 rect2 =
  range_overlaps (x_range rect1) (x_range rect2) &&
  range_overlaps (y_range rect1) (bottom rect2, bottom rect2)

overlaps rect1 rect2 =
  range_overlaps (x_range rect1) (x_range rect2) &&
  range_overlaps (y_range rect1) (y_range rect2)

contains_center: Rect -> Rect -> Bool
contains_center (x1, y1, _, _) rect2 =
  range_overlaps (x_range rect2) (x1, x1) &&
  range_overlaps (y_range rect2) (y1, y1)

x_range (x, _, w, _) =
  (x - w * 0.5, x + w * 0.5)

y_range (_, y, _, h) =
  (y - h * 0.5, y + h * 0.5)

bottom rect =
  fst (y_range rect)

range_overlaps (a, b) (c, d) =
  (max a c) < (min b d) + eps
