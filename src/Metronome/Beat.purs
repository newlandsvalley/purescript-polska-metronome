module Metronome.Beat
  (BeatNumber(..), Beat(..), Bpm, PolskaType(..), beatStart, collisionTolerance, toBeats) where

import Data.Int (toNumber)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..))
import FRP.Behavior (Behavior)
import Data.Number ((%))
import Prelude (class Eq, class Ord, class Show, map, show, (*), (-), (+), (/), (<>), (>))

data PolskaType =
    ShortFirst
  | LongFirst
  | Finnskogpols

derive instance eqPolskaType :: Eq PolskaType

instance showPolskaType :: Show PolskaType where
  show ShortFirst = "Short first beat"
  show LongFirst = "Long first beat"
  show Finnskogpols = "Finnskogpols"

-- the beat number in 3/4
data BeatNumber =
    Zero
  | One
  | Two

derive instance eqBeatNumber :: Eq BeatNumber

derive instance ordBeatNumber :: Ord BeatNumber

instance showBeatNumber :: Show BeatNumber where
  show Zero = "0"
  show One = "1"
  show Two = "2"

-- | The tempo in Beats Per Minute (bpm)
type Bpm = Int

-- | The duration of a single beat in milliseconds
beatDuration :: Bpm -> Number
beatDuration b =
  (60.0 * 1000.0 / (toNumber b))

-- | The duration of a single polska bar (3 beats) in milliseconds
polskaMeasureDuration ::  Bpm -> Number
polskaMeasureDuration b =
  (3.0 * 60.0 * 1000.0 / (toNumber b))

-- | Calculate the number of animation frames per beat assuming that
-- | they will tend to run at 60 / second. Take account of the fact
-- | that beat 1 may be skewed and thus its BPM is effectively increased.
animationFramesPerBeat :: Bpm -> Number -> BeatNumber -> Number
animationFramesPerBeat b skew beatNumber =
  let
    effectiveBpm =
      case beatNumber of
        Zero ->
          -- beat 0 is exactly at tempo
          toNumber b
        One ->
          -- beat 1 is at a faster tempo if skewed
          (toNumber b) * (1.0 + skew)
        Two ->
          -- beat 2 is exactly at tempo
          toNumber b
  in
    60.0 * 60.0 / effectiveBpm

-- | heuristic for the tolerance in which we calculate that the moving circle
-- | has collided with a marker.  We guess it must be within
-- | (slighlty more than) a single animation frame for the (possibly skewed)
-- | beat in question.
collisionTolerance :: Bpm -> Number -> BeatNumber -> Number
collisionTolerance b skew beatNumber =
  1.01 / (animationFramesPerBeat b skew beatNumber)

-- | A Beat or a proporion of a beat within a 3/4 rhythm
-- | number is the beat number (0 - 3)
-- | proportion is the proportion of the beat that is so far completed (0.0 - 1.0)
newtype Beat = Beat
  { number :: BeatNumber
  , proportion :: Number
  }

derive instance newtypeBeat :: Newtype Beat _
derive newtype instance eqBeat :: Eq Beat

instance showBeat :: Show Beat where
  show (Beat {number, proportion}) = "( BeatNumber " <> show number <>  " BeatPropotion " <> show proportion <> ")"

elapsedTimeToBeat :: PolskaType -> Number -> Bpm -> Seconds -> Beat
elapsedTimeToBeat polskaType skew b seconds =
  case polskaType of
    Finnskogpols ->
      finnskogpolsBeat skew b seconds
    _ ->
      shortFirstBeat skew b seconds

finnskogpolsBeat :: Number -> Bpm -> Seconds -> Beat
finnskogpolsBeat skew b (Seconds s) =
  let
    duration = beatDuration b
    delta = skew * duration
    thisMeasure = (s * 1000.0) % (polskaMeasureDuration b)
    Tuple number proportion =
      -- beat 2 may be skewed to start late
      if (thisMeasure > (2.0 * duration + delta)) then
        Tuple Two ( (thisMeasure - (2.0 * duration + delta )) / (duration + delta) )
      -- beat 1 is exactly at tempo
      else if (thisMeasure > duration ) then
        Tuple One ( (thisMeasure - duration) / duration)
      -- beat 0 is exactly at tempo
      else
        Tuple Zero (thisMeasure / duration )
  in
    Beat { number, proportion }

shortFirstBeat :: Number -> Bpm -> Seconds -> Beat
shortFirstBeat skew b (Seconds s) =
  let
    duration = beatDuration b
    delta = skew * duration
    thisMeasure = (s * 1000.0) % (polskaMeasureDuration b)
    Tuple number proportion =
      -- beat 2 is exactly at tempo
      if (thisMeasure > (2.0 * duration )) then
        Tuple Two ( (thisMeasure - (2.0 * duration)) / duration )
      -- beat 1 may be skewed to arrive early
      else if (thisMeasure > (duration - delta) ) then
        Tuple One ( (thisMeasure - (duration - delta)) / (duration - delta))
      -- beat 0 is exactly at tempo
      else
        Tuple Zero (thisMeasure / duration )
  in
    Beat { number, proportion }

-- | Convert a Behavior in elapsed time (seconds) to a Behavior in Beats
toBeats :: PolskaType -> Number -> Bpm -> Behavior Seconds -> Behavior Beat
toBeats polskaType skew b secs =
  map (elapsedTimeToBeat polskaType skew b) secs

-- | the beat start is the beginning of the zero beat in 3/4
beatStart :: Beat
beatStart =
  Beat
    { number : Zero
    , proportion : 0.0
    }
