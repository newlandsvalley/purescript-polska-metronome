module Metronome.Drawing
  (markers, metronome) where

import Metronome.Beat

import Color (Color, rgb, white)
import Data.Int (toNumber)
import Graphics.Drawing (Drawing, circle, rectangle, filled, fillColor)
import Math (cos, pi, sin)
import Prelude ((*), (+), (-), (/), (<>))

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
yPos = 180.0

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
-- | according to the beat and which may be skewed smaller
-- | (beat 0) or bigger (beat 1)
movingCircle :: Number -> Beat -> Drawing
movingCircle skew (Beat { number, proportion }) =
  let
    deltaRadius = skew * circleForwardRotationRadius
    -- pi / 2.0 is the ration of the circumference of a semicircle to the diameter
    -- a circle made bigger by the skew is traced more slowly
    deltaProportion = skew * proportion * pi / 2.0
    radius =
      case number of
        -- big circle back
        2 -> circleBackRotationRadius
        -- small circle forward possibly skewed bigger
        1 -> circleForwardRotationRadius + deltaRadius
        -- small circle forward  possibly skewed smaller
        _ -> circleForwardRotationRadius - deltaRadius
    centreX =
      case number of
        -- circle back is centered statically
        2 -> leftMargin + circleBackRotationRadius
        -- first circle forward will have its centre shifted left if skewed
        1 -> leftMargin + (3.0 * circleForwardRotationRadius) - deltaRadius
        -- second circle forward will also have its centre shifted left if skewed
        _ -> leftMargin + circleForwardRotationRadius - deltaRadius
    centreY = yPos
    theta =
      case number of
        2 -> proportion * pi                                -- backwards
        1 -> (1.0 - (proportion - deltaProportion)) * pi    -- forwards
        _ -> (1.0 - (proportion + deltaProportion)) * pi    -- forwards
    x = centreX + (radius * cos theta)
    y =
      case number of
        2 ->
          centreY - (radius * sin theta * 0.5)  -- down
        _ ->
          centreY + (radius * sin theta * 0.5)  -- up
  in
    filled
      (fillColor blue)
      (circle x y smallCircleRadius)

-- beat markers 0 and 2 are fixed
beatMarker :: Int -> Drawing
beatMarker beatNumber =
  let
    xPos = (toNumber beatNumber) * (2.0 * circleForwardRotationRadius)
                    + leftMargin
  in
    staticCircle xPos

-- the beat 1 marker may be skewed 'early' by an amount 'skew'
-- which should be in the range 0 <= skew <= about 0.5
skewedBeat1Marker :: Number -> Drawing
skewedBeat1Marker skew =
  let
    delta = skew * circleForwardRotationRadius
    xPos = (2.0 * (circleForwardRotationRadius - delta))
           + leftMargin
  in
    staticCircle xPos


backdrop :: Drawing
backdrop
  = filled
      (fillColor white)
      (rectangle 0.0 0.0 800.0 800.0)

markers :: Number -> Beat -> Drawing
markers skew beat =
    backdrop
      <> (beatMarker 0)
      <> (skewedBeat1Marker skew)
      <> (beatMarker 2)

metronome :: Number -> Beat -> Drawing
metronome skew beat =
  markers skew beat
    <> movingCircle skew beat
