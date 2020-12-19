module Metronome.Audio
  (BeatMap, loadBeatBuffers, playBeat) where

import Prelude

import Audio.WebAudio.AudioBufferSourceNode (defaultStartOptions, setBuffer, startBufferSource)
import Audio.WebAudio.BaseAudioContext (createBufferSource, createGain, currentTime, decodeAudioDataAsync, destination)
import Audio.WebAudio.GainNode (setGain)
import Audio.WebAudio.Types (AudioContext, AudioBuffer, connect)
import Control.Parallel (parallel, sequential)
import Data.Array ((!!), concat, singleton)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Affjax (request, defaultRequest)
import Affjax.ResponseFormat as ResponseFormat
import Metronome.Beat (BeatNumber(..), Beat(..), Bpm, collisionTolerance)

-- | an index into the buffer sound for each Beat number
type BeatMap = Map BeatNumber AudioBuffer

-- | Beat Sounds in Web-Audio
-- | The use should supply a url path to where the sound files reside
-- | together with an array of three file names for the sounds of each beat:
-- | Strong, Medium and Weak.

-- | load a single sound buffer resource and decode it
loadSoundBuffer
  :: AudioContext
  -> String
  -> String
  -> Aff (Array AudioBuffer)
loadSoundBuffer ctx path name =
  let
    filename = path <> "/" <> name
  in do
    res <- request $ defaultRequest
            { url = filename
            , method = Left GET
            , responseFormat = ResponseFormat.arrayBuffer  }

    case res <#> _.body of
        Left err ->
          pure []
        Right body -> do
         buffer <- decodeAudioDataAsync ctx body
         pure $ singleton buffer

-- | load and decode an array of audio buffers from a set of resources
loadSoundBuffers
  :: AudioContext
  -> String
  -> (Array String)
  -> Aff (Array AudioBuffer)
loadSoundBuffers ctx path names = do
  arrays <- sequential $ traverse (\name -> parallel (loadSoundBuffer ctx path name)) names
  pure $ concat arrays

-- | load all the 'beat' buffers and place them in a map keyed by beat name
loadBeatBuffers
  :: AudioContext
  -> String
  -> (Array String)
  -> Aff BeatMap
loadBeatBuffers ctx path names = do
  -- buffers <- loadSoundBuffers ctx "assets/audio" ["hightom.mp3", "tom.mp3", "hihat.mp3"]
  buffers <- loadSoundBuffers ctx path names
  let
    map0 = maybeInsert Zero (buffers !! 0) empty
    map1 = maybeInsert One (buffers !! 1) map0
    map2 = maybeInsert Two (buffers !! 2) map1
  pure map2

maybeInsert :: forall k v. Ord k => k -> Maybe v -> Map k v -> Map k v
maybeInsert k mv map =
  case mv of
    Just v -> insert k v map
    _ -> map


-- | play the requested beat through the audio buffer
playBeat
  :: AudioContext
  -> Bpm
  -> Number
  -> BeatMap
  -> Beat
  -> Effect Unit
playBeat ctx bpm skew beatMap (Beat { number, proportion }) =
  if (proportion > collisionTolerance bpm skew number) then
    pure unit
  else
    let
      volume =
        -- make the second beat quieter than the other two
        case number of
          One -> 0.4
          _ -> 1.0
    in
      case lookup number beatMap of
        Just buffer ->
          do
            startTime <- currentTime ctx
            src <- createBufferSource ctx
            gain <- createGain ctx
            _ <- setGain volume gain
            dst <- destination ctx
            _ <- connect src gain
            _ <- connect gain dst
            _ <- setBuffer buffer src
            startBufferSource defaultStartOptions src
        _ ->
          pure unit
