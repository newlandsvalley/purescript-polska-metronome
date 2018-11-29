module Metronome.Audio where

import Prelude

import Audio.WebAudio.AudioBufferSourceNode (defaultStartOptions, setBuffer, startBufferSource)
import Audio.WebAudio.BaseAudioContext (createBufferSource, currentTime, decodeAudioDataAsync, destination)
import Audio.WebAudio.Types (AudioContext, AudioBuffer, connect)
import Control.Parallel (parallel, sequential)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Network.HTTP.Affjax (affjax, defaultRequest)
import Network.HTTP.Affjax.Response as Response
import Metronome.Beat (Beat(..))

-- | an index into the buffer sound for each Beat number
type BeatMap = Map Int AudioBuffer

-- | Beat Sounds in Web-Audio
-- | The use should supply a url path to where the sound files reside
-- | together with an array of three file names for the sounds of each beat:
-- | Strong, Medium and Weak.

-- | load a single sound buffer resource and decode it
loadSoundBuffer
  :: AudioContext
  -> String
  -> String
  -> Aff AudioBuffer
loadSoundBuffer ctx path name =
  let
    filename = path <> "/" <> name
  in do
    res <- affjax Response.arrayBuffer $ defaultRequest { url = filename, method = Left GET }
    buffer <- decodeAudioDataAsync ctx res.response
    pure buffer

-- | load and decode an array of audio buffers from a set of resources
loadSoundBuffers
  :: AudioContext
  -> String
  -> (Array String)
  -> Aff (Array AudioBuffer)
loadSoundBuffers ctx path names =
  sequential $ traverse (\name -> parallel (loadSoundBuffer ctx path name)) names

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
    map0 = maybeInsert 0 (buffers !! 0) empty
    map1 = maybeInsert 1 (buffers !! 1) map0
    map2 = maybeInsert 2 (buffers !! 2) map1
  pure map2

maybeInsert :: forall k v. Ord k => k -> Maybe v -> Map k v -> Map k v
maybeInsert k mv map =
  case mv of
    Just v -> insert k v map
    _ -> map


-- | play the requested beat through the audio buffer
playBeat
  :: AudioContext
  -> BeatMap
  -> Beat
  -> Effect Unit
playBeat ctx beatMap (Beat { number, proportion }) =
  if (proportion > 0.035 ) then
    pure unit
  else
    case lookup number beatMap of
      Just buffer ->
        do
          startTime <- currentTime ctx
          src <- createBufferSource ctx
          dst <- destination ctx
          _ <- connect src dst
          _ <- setBuffer buffer src
          startBufferSource defaultStartOptions src
      _ ->
        pure unit
