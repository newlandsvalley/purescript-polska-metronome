module Metronome.Beat
  (Beat(..), Bpm, beatStart, collisionTolerance, toBeats) where

import Data.Int (toNumber)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..))
import FRP.Behavior (Behavior)
import Math ((%))
import Prelude (class Eq, class Show, map, show, (*), (-), (+), (/), (<>), (>))

-- | The tempo in Beats Per Minute (bpm)
type Bpm = Int

-- | the duration of a single beat in milliseconds
beatDuration :: Bpm -> Number
beatDuration b =
  (60.0 * 1000.0 / (toNumber b))

-- | the duration of a single polska bar (3 beats) in milliseconds
polskaMeasureDuration ::  Bpm -> Number
polskaMeasureDuration b =
  (3.0 * 60.0 * 1000.0 / (toNumber b))

-- | calculate the number of animation frames per beat assuming that
-- | they will tend to run at 60 / second and that the beats are not
-- | skewed, but with skew, the beat 1 has more BPM.
animationFramesPerBeat :: Bpm -> Number -> Int-> Number
animationFramesPerBeat b skew beatNumber =
  let
    effectiveBpm =
      case beatNumber of
        0 ->
          -- beat 0 is exactly at tempo
          toNumber b
        1 ->
          -- beat 1 is at a faster tempo if skewed
          (toNumber b) * (1.0 + skew)
        _ ->
          -- beat 2 is exactly at tempo
          toNumber b
  in
    60.0 * 60.0 / effectiveBpm

-- | heuristic for the tolerance in which we calculate that the moving circle
-- | has collided with a marker.  We guess it must be within
-- | (slighlty more than) a single animation frame for the (possibly skewed)
-- | beat in question.
collisionTolerance :: Bpm -> Number -> Int -> Number
collisionTolerance b skew beatNumber =
  1.01 / (animationFramesPerBeat b skew beatNumber)

-- | A Beat or a proporion of a beat within a 3/4 rhythm
-- | number is the beat number (0 - 3)
-- | proportion is the proportion of the beat that is so far completed (0.0 - 1.0)
newtype Beat = Beat
  { number :: Int
  , proportion :: Number
  }

derive instance newtypeBeat :: Newtype Beat _
derive newtype instance eqBeat :: Eq Beat

instance showBeat :: Show Beat where
  show (Beat {number, proportion}) = "( BeatNumber " <> show number <>  " BeatPropotion " <> show proportion <> ")"

elapsedTimeToBeat :: Number -> Bpm -> Seconds -> Beat
elapsedTimeToBeat skew b (Seconds s) =
  let
    duration = beatDuration b
    delta = skew * duration
    thisMeasure = (s * 1000.0) % (polskaMeasureDuration b)
    Tuple number proportion =
      -- beat 2 is exactly at tempo
      if (thisMeasure > (2.0 * duration )) then
        Tuple 2 ( (thisMeasure - (2.0 * duration)) / duration )
      -- beat 1 may be skewed to arrive early
      else if (thisMeasure > (duration - delta) ) then
        Tuple 1 ( (thisMeasure - (duration - delta)) / (duration - delta))
      -- beat 0 is exactly at tempo
      else
        Tuple 0 (thisMeasure / duration )
  in
    Beat { number, proportion }

-- | convert a Behavior in elapsed time (seconds) to a Behavior in Beats
toBeats :: Number -> Bpm -> Behavior Seconds -> Behavior Beat
toBeats skew b secs =
  map (elapsedTimeToBeat skew b) secs

-- | the beat start is the beginning of the first beat in 3/4
beatStart :: Beat
beatStart =
  Beat
    { number : 0
    , proportion : 0.0
    }
