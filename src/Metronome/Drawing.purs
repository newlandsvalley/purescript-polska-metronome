module Metronome.Drawing
  ( canvasHeight
  , canvasWidth
  , markers
  , metronome) where

import Metronome.Beat

import Color (Color, rgb, white, graytone)
import Data.Int (toNumber)
import Data.Number (cos, pi, sin)
import Graphics.Drawing (Drawing, circle, fillColor, filled, rectangle)
import Prelude ((*), (+), (-), (/), (<>), (&&), (==), (<))

canvasHeight :: Int 
canvasHeight = 325

canvasWidth :: Int 
canvasWidth = 700

-- the position of a beat marker on the canvas
data MarkerPosition =
    Left
  | Middle
  | Right

-- | the radius of the outer halo when a collision occurs
outerHaloRadius :: Number -> Number
outerHaloRadius scale =
  (innerHaloRadius scale) + (3.0 * scale)

-- | the radius of the inner halo when a collision occurs
innerHaloRadius :: Number -> Number
innerHaloRadius scale =
  (bigCircleRadius scale) + (3.0 * scale)

-- | the radius of the outline of a big static circle
bigCircleRadius :: Number -> Number
bigCircleRadius scale =
  (smallCircleRadius scale) + (5.0 * scale)

-- | the radius of the smaller moving circle
-- | as well as the interior ring of the big static circle
smallCircleRadius :: Number -> Number
smallCircleRadius scale = 
  23.0 * scale

-- | the radius of rotation of a circle below the linen when the movable circle is centered
-- | forwards on beats 0 and 1  (normalpolska)
-- | backwards on beats 1 and 2  (finnskohpols)
circleBelowRotationRadius :: Number -> Number
circleBelowRotationRadius scale = 
  150.0 * scale

-- | the radius of the rotation of a circle above the line
-- | which must be twice that of the lower circle radius.
-- | back on beat 2 (normalpolska)
-- | forwrds on beat 0 (finnskogpols)
circleAboveRotationRadius :: Number -> Number
circleAboveRotationRadius scale =
  2.0 * (circleBelowRotationRadius scale)

-- | the leftmost margin to the centre of the first of our three static circles
leftMargin :: Number -> Number
leftMargin scale =
  50.0 * scale

-- | everything is placed relative to a horizontal axis at the same vertical y coordinate
-- | i.e. the centre of each fixed circle lies on this axis
yPosAxis :: Number -> Number
yPosAxis scale = 
  180.0 * scale

green :: Color
green = rgb 102 153 102

olive :: Color
olive = rgb 103 78 7

gray :: Color
gray = rgb 160 160 160

-- | Create a drawing of a static circle as a ring
-- | but which is enhanced with  'halo' if it has recently collided with the ball
staticCircle :: Number -> Boolean -> Number -> Drawing
staticCircle x isCollided scale =
  if isCollided then
    filled
      (fillColor gray)
      (circle x (yPosAxis scale) (outerHaloRadius scale))
    <>
      filled
        (fillColor white)
        (circle x (yPosAxis scale) (innerHaloRadius scale))
    <>
      uncollidedCircle x scale
  else
    uncollidedCircle x scale

-- | create a circle representing a beat marker where there is no collision
-- | with the moving ball
uncollidedCircle :: Number -> Number -> Drawing
uncollidedCircle x scale =
    filled
      (fillColor olive)
      (circle x (yPosAxis scale) (bigCircleRadius scale))
  <>
    filled
      (fillColor white)
      (circle x (yPosAxis scale) (smallCircleRadius scale))

-- | Create a drawing of a moving circle which alters position
-- | according to the beat and which may be skewed smaller
-- | (beat 0) or bigger (beat 1)
movingCircle :: PolskaType -> Number -> Beat -> Number -> Drawing
movingCircle polskaType skew beat scale =
  case polskaType of
    ShortFirst ->
      movingCircleShortFirst skew beat scale
    LongFirst ->  -- identical to ShortFirst but with skew = (- skew)
      movingCircleShortFirst skew beat scale
    Finnskogpols ->
      movingCircleFinnskogpols skew beat scale


movingCircleShortFirst :: Number -> Beat -> Number -> Drawing
movingCircleShortFirst skew (Beat { number, proportion }) scale =
  let
    deltaRadius = skew * (circleBelowRotationRadius scale)
    -- pi / 2.0 is the ration of the circumference of a semicircle to the diameter
    -- a circle made bigger by the skew is traced more slowly
    deltaProportion = skew * proportion * pi / 2.0
    radius =
      case number of
        -- big circle back
        Two -> (circleAboveRotationRadius scale)
        -- small circle forward possibly skewed bigger
        One -> (circleBelowRotationRadius scale) + deltaRadius
        -- small circle forward  possibly skewed smaller
        Zero -> (circleBelowRotationRadius scale) - deltaRadius
    centreX =
      case number of
        -- circle back is centered statically
        Two -> (leftMargin scale) + (circleAboveRotationRadius scale)
        -- first circle forward will have its centre shifted left if skewed
        One -> (leftMargin scale) + (3.0 * (circleBelowRotationRadius scale)) - deltaRadius
        -- second circle forward will also have its centre shifted left if skewed
        Zero -> (leftMargin scale) + (circleBelowRotationRadius scale) - deltaRadius
    centreY = yPosAxis scale
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
      (circle x y (smallCircleRadius scale))

movingCircleFinnskogpols :: Number -> Beat -> Number -> Drawing
movingCircleFinnskogpols skew (Beat { number, proportion }) scale =
  let
    deltaRadius = skew * (circleBelowRotationRadius scale)
    -- pi / 2.0 is the ration of the circumference of a semicircle to the diameter
    -- a circle made bigger by the skew is traced more slowly
    deltaProportion = skew * proportion * pi / 2.0
    radius =
      case number of
        -- big circle forward
        Zero -> circleAboveRotationRadius scale
        -- small circle back possibly skewed bigger
        One -> (circleBelowRotationRadius scale) + deltaRadius
        -- small circle back  possibly skewed smaller
        Two -> (circleBelowRotationRadius scale) - deltaRadius
    centreX =
      case number of
        -- circle forward is centered statically
        Zero -> (leftMargin scale) + (circleAboveRotationRadius scale)
        -- first circle back will have its centre shifted left if skewed
        One -> (leftMargin scale) + (3.0 * (circleBelowRotationRadius scale)) - deltaRadius
        -- second circle back will also have its centre shifted left if skewed
        Two -> (leftMargin scale) + (circleBelowRotationRadius scale) - deltaRadius
    centreY = yPosAxis scale
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
      (circle x y (smallCircleRadius scale))


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
beatMarker :: MarkerPosition -> Boolean -> Number -> Drawing
beatMarker markerPosition isCollided scale =
  let
    xPos = (markerPositionNumber markerPosition) * (2.0 * (circleBelowRotationRadius scale))
                    + (leftMargin scale)
  in
    staticCircle xPos isCollided scale

-- the middle marker may be skewed 'early' by an amount 'skew'
-- which should be in the range 0 <= skew <= about 0.5
skewedMiddleMarker :: Number -> Boolean -> Number -> Drawing
skewedMiddleMarker skew isCollided scale =
  let
    delta = skew * (circleBelowRotationRadius scale)
    xPos = (2.0 * ((circleBelowRotationRadius scale) - delta))
                    + (leftMargin scale)
  in
    staticCircle xPos isCollided scale

backdrop :: Number -> Drawing
backdrop scale
  = filled 
      -- debug so we can see it - (fillColor (graytone 0.8))
      (fillColor white)
      (rectangle 0.0 0.0 ((toNumber canvasWidth) * scale) ((toNumber canvasHeight) * scale))

-- | Work out if the moving ball has collided with the narker for the given
-- | beat number.  This happens if the current beat coincides and the
-- | proportion of the beat that has elapsed is small.
markerCollided :: Boolean -> BeatNumber -> Beat -> Boolean
markerCollided isActive beatNumber (Beat { number, proportion }) =
  if isActive then
    (beatNumber == number)
      && (proportion < 0.2)
  else
    false

-- | draw an individual marker (number 0, 1 or 2)
-- | work out if it has collided or not to determine whether or not
-- | to draw the 'halo'
drawMarker :: Boolean -> PolskaType -> BeatNumber -> Number -> Beat -> Number -> Drawing
drawMarker isActive polskaType beatNumber skew beat scale =
  let
    isCollided :: Boolean
    isCollided = markerCollided isActive beatNumber beat
    markerPosition = beatNumberToMarkerPosition polskaType beatNumber
  in
    case markerPosition of
      Middle ->
        skewedMiddleMarker skew isCollided scale
      _ ->
        beatMarker markerPosition isCollided scale


-- | Draw the three static markers which are placeholders for each beat
-- | Used in cases where the animation is stopped.
markers :: PolskaType -> Number -> Number -> Drawing
markers polskaType skew scale =
  -- bpm and beatStart are irrelevant when the animation is inactive
  -- They're  only used to detect collisions
  movingMarkers polskaType false skew beatStart scale

-- | Draw the markers, but taking account of whether or not the animation is
-- | active, in which case a marker will display a 'halo' if it subjected to
-- | a collision
movingMarkers  :: PolskaType -> Boolean -> Number -> Beat -> Number -> Drawing
movingMarkers polskaType isActive skew beat scale =
  backdrop scale
      <> drawMarker isActive polskaType Zero skew beat scale
      <> drawMarker isActive polskaType One skew beat scale
      <> drawMarker isActive polskaType Two skew beat scale


-- | Draw the active metronome at the particular beat in question.
-- | although bpm is not used, we retain it in case we can improve collision detection
metronome :: PolskaType -> Number -> Bpm -> Beat -> Number -> Drawing
metronome polskaType skew bpm beat scale =
  movingMarkers polskaType true skew beat scale
    <> movingCircle polskaType skew beat scale
