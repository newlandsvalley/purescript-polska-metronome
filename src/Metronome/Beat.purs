module Metronome.Beat
  (Beat(..), Bpm, beatStart, toBeats) where

import Prelude (class Eq, class Show, (*), (-), (/), (>), (<>), map, show)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Math ((%))
import Data.Newtype (class Newtype)
import Data.Time.Duration (Seconds(..))
import FRP.Behavior (Behavior)

-- | The tempo in Beats Per Minute (bpm)
type Bpm = Int

beatDuration :: Bpm -> Number
beatDuration b =
  (60.0 * 1000.0 / (toNumber b))

polskaMeasureDuration ::  Bpm -> Number
polskaMeasureDuration b =
  (3.0 * 60.0 * 1000.0 / (toNumber b))

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
