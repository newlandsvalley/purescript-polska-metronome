module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Fiber, launchAff)
import Effect.Class (liftEffect)
import Data.Maybe (fromJust)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Metronome.Audio (loadBeatBuffers, playBeat)
import Graphics.Drawing (render)
import FRP.Behavior (animate)
import FRP.Behavior.Time (seconds)
import Partial.Unsafe (unsafePartial)

import Metronome.Drawing (markers)
import Metronome.Beat (bpm, toBeats)


main :: Effect (Fiber Unit)
main =
  launchAff $ do
    audioCtx <- liftEffect newAudioContext
    beatMap <- loadBeatBuffers audioCtx "assets/audio" ["hightom.mp3", "tom.mp3", "hihat.mp3"]
    mcanvas <- liftEffect $ getCanvasElementById "canvas"
    let
      canvas = unsafePartial (fromJust mcanvas)
      skew = 0.3
    graphicsCtx <- liftEffect $ getContext2D canvas
    _ <- liftEffect $ animate (toBeats skew bpm seconds) \beat -> do
           _ <- render graphicsCtx (markers skew beat)
           playBeat audioCtx beatMap beat
    pure unit
