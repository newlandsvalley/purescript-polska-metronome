module Metronome.Drawing where

import Metronome.Beat

import Color (Color, rgb, white)
import Data.Int (toNumber)
import Graphics.Drawing (Drawing, circle, rectangle, filled, fillColor)
import Math ((%), cos, pi, sin)
import Prelude ((*), (+), (-), (<>))

-- | the radius of the outline of a big static circle
bigCircleRadius :: Number
bigCircleRadius = 30.0

-- | the radius of the smaller moving circle
-- | as well as the interior ring of the big static circle
smallCircleRadius :: Number
smallCircleRadius = 23.0

-- | the radius of rotation forwards on beats 0 and 1
circleForwardRotationRadius :: Number
circleForwardRotationRadius = 150.0

-- | the radius of the rotation to get back on beat 2
circleBackRotationRadius :: Number
circleBackRotationRadius =
  2.0 * circleForwardRotationRadius

-- | the leftmost margin for the first of our three static circles
leftMargin :: Number
leftMargin =
  100.0

-- | everything is placed relative to the same vertical y coordinate
yPos :: Number
yPos = 200.0

red :: Color
red = rgb 200 0 0

blue :: Color
blue = rgb 0 0 200

-- | Create a drawing of a static circle as a ring
staticCircle :: Number -> Drawing
staticCircle x =
  filled
    (fillColor red)
    (circle x yPos bigCircleRadius)
  <>
    filled
      (fillColor white)
      (circle x yPos smallCircleRadius)

-- | Create a drawing of a moving circle which alters position
-- | according to the beat
movingCircle :: Beat -> Drawing
movingCircle (Beat { number, proportion }) =
  let
    radius =
      case number of
        2 -> circleBackRotationRadius      -- big circle back
        _ -> circleForwardRotationRadius   -- small circle forward
    centreX =
      case number of
        0 -> leftMargin + circleForwardRotationRadius
        1 -> leftMargin + (3.0 * circleForwardRotationRadius)
        _ -> leftMargin + circleBackRotationRadius
    centreY = yPos
    theta =
      case number of
        2 -> proportion * pi            -- backwards
        _ -> (1.0 - proportion) * pi    -- forwards
    x = centreX + (radius * cos theta)
    y =
      case number of
        2 ->
          centreY - (radius * sin theta * 0.5)  -- down
        _ ->
          centreY + (radius * sin theta * 0.5)  -- uo
  in
    filled
      (fillColor blue)
      (circle x y smallCircleRadius)

beatMarker :: Int -> Drawing
beatMarker beatNumber =
  let
    xPos = (toNumber beatNumber) % 3.0 * (2.0 * circleForwardRotationRadius)
           + leftMargin
  in
    staticCircle xPos

backdrop :: Drawing
backdrop
  = filled
      (fillColor white)
      (rectangle 0.0 0.0 800.0 800.0)

markers :: Beat -> Drawing
markers beat =
    backdrop
      <> (beatMarker 0)
      <> (beatMarker 1)
      <> (beatMarker 2)
      <> movingCircle beat
