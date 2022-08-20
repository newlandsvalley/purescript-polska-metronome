module Metronome.Drawing
  (markers, metronome) where

import Metronome.Beat

import Color (Color, rgb, white)
import Graphics.Drawing (Drawing, circle, rectangle, filled, fillColor)
import Data.Number (cos, pi, sin)
import Prelude ((*), (+), (-), (/), (<>), (&&), (==), (<))

-- the position of a beat marker on the canvas
data MarkerPosition =
    Left
  | Middle
  | Right

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
  smallCircleRadius + 5.0

-- | the radius of the smaller moving circle
-- | as well as the interior ring of the big static circle
smallCircleRadius :: Number
smallCircleRadius = 23.0

-- | the radius of rotation of a circle below the line
-- | forwards on beats 0 and 1  (normalpolska)
-- | backwards on beats 1 and 2  (finnskohpols)
circleBelowRotationRadius :: Number
circleBelowRotationRadius = 150.0

-- | the radius of the rotation of a circle above the line
-- | back on beat 2 (normalpolska)
-- | forwrds on beat 0 (finnskogpols)
circleAboveRotationRadius :: Number
circleAboveRotationRadius =
  2.0 * circleBelowRotationRadius

-- | the leftmost margin to the centre of the first of our three static circles
leftMargin :: Number
leftMargin =
  50.0

-- | everything is placed relative to the same vertical y coordinate
yPos :: Number
yPos = 180.0

green :: Color
green = rgb 102 153 102

olive :: Color
olive = rgb 103 78 7

gray :: Color
gray = rgb 160 160 160

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
      (fillColor olive)
      (circle x yPos bigCircleRadius)
  <>
    filled
      (fillColor white)
      (circle x yPos smallCircleRadius)

-- | Create a drawing of a moving circle which alters position
-- | according to the beat and which may be skewed smaller
-- | (beat 0) or bigger (beat 1)
movingCircle :: PolskaType -> Number -> Beat -> Drawing
movingCircle polskaType skew beat =
  case polskaType of
    ShortFirst ->
      movingCircleShortFirst skew beat
    LongFirst ->  -- identical to ShortFirst but with skew = (- skew)
      movingCircleShortFirst skew beat
    Finnskogpols ->
      movingCircleFinnskogpols skew beat


movingCircleShortFirst :: Number -> Beat -> Drawing
movingCircleShortFirst skew (Beat { number, proportion }) =
  let
    deltaRadius = skew * circleBelowRotationRadius
    -- pi / 2.0 is the ration of the circumference of a semicircle to the diameter
    -- a circle made bigger by the skew is traced more slowly
    deltaProportion = skew * proportion * pi / 2.0
    radius =
      case number of
        -- big circle back
        Two -> circleAboveRotationRadius
        -- small circle forward possibly skewed bigger
        One -> circleBelowRotationRadius + deltaRadius
        -- small circle forward  possibly skewed smaller
        Zero -> circleBelowRotationRadius - deltaRadius
    centreX =
      case number of
        -- circle back is centered statically
        Two -> leftMargin + circleAboveRotationRadius
        -- first circle forward will have its centre shifted left if skewed
        One -> leftMargin + (3.0 * circleBelowRotationRadius) - deltaRadius
        -- second circle forward will also have its centre shifted left if skewed
        Zero -> leftMargin + circleBelowRotationRadius - deltaRadius
    centreY = yPos
    theta =
      case number of
        Two -> proportion * pi                                -- backwards
        One -> (1.0 - (proportion - deltaProportion)) * pi    -- forwards
        Zero -> (1.0 - (proportion + deltaProportion)) * pi   -- forwards
    x = centreX + (radius * cos theta)
    y =
      case number of
        Two ->
          centreY - (radius * sin theta * 0.5)  -- above
        _ ->
          centreY + (radius * sin theta * 0.5)  -- below
  in
    filled
      (fillColor green)
      (circle x y smallCircleRadius)

movingCircleFinnskogpols :: Number -> Beat -> Drawing
movingCircleFinnskogpols skew (Beat { number, proportion }) =
  let
    deltaRadius = skew * circleBelowRotationRadius
    -- pi / 2.0 is the ration of the circumference of a semicircle to the diameter
    -- a circle made bigger by the skew is traced more slowly
    deltaProportion = skew * proportion * pi / 2.0
    radius =
      case number of
        -- big circle forward
        Zero -> circleAboveRotationRadius
        -- small circle back possibly skewed bigger
        One -> circleBelowRotationRadius + deltaRadius
        -- small circle back  possibly skewed smaller
        Two -> circleBelowRotationRadius - deltaRadius
    centreX =
      case number of
        -- circle forward is centered statically
        Zero -> leftMargin + circleAboveRotationRadius
        -- first circle back will have its centre shifted left if skewed
        One -> leftMargin + (3.0 * circleBelowRotationRadius) - deltaRadius
        -- second circle back will also have its centre shifted left if skewed
        Two -> leftMargin + circleBelowRotationRadius - deltaRadius
    centreY = yPos
    theta =
      case number of
        Zero -> (1.0 - proportion) * pi               -- forwwards
        One -> (proportion - deltaProportion) * pi    -- back
        Two -> (proportion + deltaProportion) * pi    -- back
    x = centreX + (radius * cos theta)
    y =
      case number of
        Zero ->
          centreY - (radius * sin theta * 0.5)  -- above
        _ ->
          centreY + (radius * sin theta * 0.5)  -- below
  in
    filled
      (fillColor green)
      (circle x y smallCircleRadius)


beatNumberToMarkerPosition :: PolskaType -> BeatNumber -> MarkerPosition
beatNumberToMarkerPosition polskaType beatNumber =
  case beatNumber of
    Zero ->
      Left
    One ->
      case polskaType of
        Finnskogpols ->
          Right
        ShortFirst ->
          Middle
        LongFirst ->
          Middle
    Two ->
      case polskaType of
        Finnskogpols ->
          Middle
        ShortFirst ->
          Right
        LongFirst ->
          Right

markerPositionNumber :: MarkerPosition -> Number
markerPositionNumber markerPosition =
  case markerPosition of
    Left ->
      0.0
    Middle ->
      1.0
    Right ->
      2.0

-- beat markers left and Right are fixed
beatMarker :: MarkerPosition -> Boolean -> Drawing
beatMarker markerPosition isCollided =
  let
    xPos = (markerPositionNumber markerPosition) * (2.0 * circleBelowRotationRadius)
                    + leftMargin
  in
    staticCircle xPos isCollided

-- the middle marker may be skewed 'early' by an amount 'skew'
-- which should be in the range 0 <= skew <= about 0.5
skewedMiddleMarker :: Number -> Boolean -> Drawing
skewedMiddleMarker skew isCollided =
  let
    delta = skew * circleBelowRotationRadius
    xPos = (2.0 * (circleBelowRotationRadius - delta))
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
markerCollided :: Boolean -> BeatNumber -> Number -> Bpm -> Beat -> Boolean
markerCollided isActive beatNumber skew bpm (Beat { number, proportion }) =
  if isActive then
    (beatNumber == number)
      && (proportion < 0.2)
  else
    false

-- | draw an individual marker (number 0, 1 or 2)
-- | work out if it has collided or not to determine whether or not
-- | to draw the 'halo'
drawMarker :: Boolean -> PolskaType -> BeatNumber -> Number -> Bpm -> Beat -> Drawing
drawMarker isActive polskaType beatNumber  skew bpm beat =
  let
    isCollided :: Boolean
    isCollided = markerCollided isActive beatNumber skew bpm beat
    markerPosition = beatNumberToMarkerPosition polskaType beatNumber
  in
    case markerPosition of
      Middle ->
        skewedMiddleMarker skew isCollided
      _ ->
        beatMarker markerPosition isCollided


-- | Draw the three static markers which are placeholders for each beat
-- | Used in cases where the animation is stopped.
markers :: PolskaType -> Number -> Drawing
markers polskaType skew =
  -- bpm and beatStart are irrelevant when the animation is inactive
  -- They're  only used to detect collisions
  movingMarkers polskaType false skew 120 beatStart

-- | Draw the markers, but taking account of whether or not the animation is
-- | active, in which case a marker will display a 'halo' if it subjected to
-- | a collision
movingMarkers  :: PolskaType -> Boolean -> Number -> Bpm -> Beat -> Drawing
movingMarkers polskaType isActive skew bpm beat =
  backdrop
      <> drawMarker isActive polskaType Zero skew bpm beat
      <> drawMarker isActive polskaType One skew bpm beat
      <> drawMarker isActive polskaType Two skew bpm beat


-- | Draw the active metronome at the particular beat in question.
metronome :: PolskaType -> Number -> Bpm -> Beat -> Drawing
metronome polskaType skew bpm beat =
  movingMarkers polskaType true skew bpm beat
    <> movingCircle polskaType skew beat
