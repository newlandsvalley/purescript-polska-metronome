module Metronome.Drawing
  (markers, metronome) where

import Metronome.Beat

import Color (Color, rgb, white)
import Data.Int (toNumber)
import Graphics.Drawing (Drawing, circle, rectangle, filled, fillColor)
import Math (cos, pi, sin)
import Prelude ((*), (+), (-), (/), (<>), (&&), (==), (<))

-- | the radius of the outer halo when a collision occurs
outerHaloRadius :: Number
outerHaloRadius =
  innerHaloRadius + 3.0

-- | the radius of the inner halo when a collision occurs
innerHaloRadius :: Number
innerHaloRadius =
  bigCircleRadius + 3.0

-- | the radius of the outline of a big static circle
bigCircleRadius :: Number
bigCircleRadius =
  smallCircleRadius + 3.0

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

-- | the leftmost margin to the centre of the first of our three static circles
leftMargin :: Number
leftMargin =
  50.0

-- | everything is placed relative to the same vertical y coordinate
yPos :: Number
yPos = 180.0

red :: Color
red = rgb 200 0 0

blue :: Color
blue = rgb 0 0 200

gray :: Color
gray = rgb 160 160 160
-- gray = rgb 150 150 150

-- | Create a drawing of a static circle as a ring
-- | but which is enhanced with  'halo' if it has recently collided with the ball
staticCircle :: Number -> Boolean -> Drawing
staticCircle x isCollided =
  if isCollided then
    filled
      (fillColor gray)
      (circle x yPos outerHaloRadius)
    <>
      filled
        (fillColor white)
        (circle x yPos innerHaloRadius)
    <>
      uncollidedCircle x
  else
    uncollidedCircle x

-- | create a circle representing a beat marker where there is no collision
-- | with the moving ball
uncollidedCircle :: Number -> Drawing
uncollidedCircle x =
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
beatMarker :: Int -> Boolean -> Drawing
beatMarker beatNumber isCollided =
  let
    xPos = (toNumber beatNumber) * (2.0 * circleForwardRotationRadius)
                    + leftMargin
  in
    staticCircle xPos isCollided

-- the beat 1 marker may be skewed 'early' by an amount 'skew'
-- which should be in the range 0 <= skew <= about 0.5
skewedBeat1Marker :: Number -> Boolean -> Drawing
skewedBeat1Marker skew isCollided =
  let
    delta = skew * circleForwardRotationRadius
    xPos = (2.0 * (circleForwardRotationRadius - delta))
                    + leftMargin
  in
    staticCircle xPos isCollided


backdrop :: Drawing
backdrop
  = filled
      (fillColor white)
      (rectangle 0.0 0.0 800.0 350.0)

-- | Work out if the moving ball has collided with the narker for the given
-- | beat number.  This happens if the current beat coincides and the
-- | proportion of the beat that has elapsed is small.
markerCollided :: Boolean -> Int -> Number -> Bpm -> Beat -> Boolean
markerCollided isActive beatNumber skew bpm (Beat { number, proportion }) =
  if isActive then
    (beatNumber == number)
      && (proportion < 0.2)
  else
    false

-- | draw an individual marker (number 0, 1 or 2)
-- | work out if it has collided or not to determine whether or not
-- | to draw the 'halo'
drawMarker :: Boolean -> Int -> Number -> Bpm -> Beat -> Drawing
drawMarker isActive beatNumber skew bpm beat =
  let
    isCollided :: Boolean
    isCollided = markerCollided isActive beatNumber skew bpm beat
  in
    case beatNumber of
      1 ->
        skewedBeat1Marker skew isCollided
      _ ->
        beatMarker beatNumber isCollided


-- | Draw the three static markers which are placeholders for each beat
-- | Used in cases where the animation is stopped.
markers :: Number -> Drawing
markers skew =
  -- bpm and beatStart are irrelevant when the animation is inactive
  -- They're  only used to detect collisions
  movingMarkers false skew 120 beatStart

-- | Draw the markers, but taking account of whether or not the animation is
-- | active, in which case a marker will display a 'halo' if it subjected to
-- | a collision
movingMarkers  :: Boolean -> Number -> Bpm -> Beat -> Drawing
movingMarkers isActive skew bpm beat =
    backdrop
      <> drawMarker isActive 0 skew bpm beat
      <> drawMarker isActive 1 skew bpm beat
      <> drawMarker isActive 2 skew bpm beat


-- | Draw the active metronome at the particular beat in question.
metronome :: Number -> Bpm -> Beat -> Drawing
metronome skew bpm beat =
  movingMarkers true skew bpm beat
    <> movingCircle skew beat
