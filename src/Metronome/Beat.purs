module Metronome.Beat
  (Beat(..), Bpm, bpm, toBeats) where

import Prelude (class Eq, class Show, (*), (-), (/), (>), ($), (<>), map, show)
import Data.Int (toNumber, floor)
import Data.Tuple (Tuple(..))
import Math ((%))
import Data.Newtype (class Newtype)
import Data.Time.Duration (Seconds(..))
import FRP.Behavior (Behavior)

-- | we use strict 3/4 time at the moment
-- | (not skewed polska time)

type Bpm = Int

-- | we use 1/4=120 at the moment
-- | we'll allow the user to vary this later
bpm :: Bpm
bpm = 120

beatDuration :: Bpm -> Number
beatDuration b =
  (60.0 * 1000.0 / (toNumber b))

polskaMeasureDuration ::  Bpm -> Number
polskaMeasureDuration b =
  (3.0 * 60.0 * 1000.0 / (toNumber b))

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


{-
elapsedTimeToBeat :: Bpm -> Seconds -> Beat
elapsedTimeToBeat b (Seconds s) =
  let
    thisMeasure = (s * 1000.0) % (polskaMeasureDuration b)
  in
    Beat { number : (floor $ thisMeasure / beatDuration b), proportion : ((thisMeasure % beatDuration b) /  (beatDuration b)) }
-}


toBeats :: Number -> Bpm -> Behavior Seconds -> Behavior Beat
toBeats skew b secs =
  map (elapsedTimeToBeat skew b) secs
