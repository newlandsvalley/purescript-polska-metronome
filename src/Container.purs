module Container where

import Prelude
import Global (readFloat)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.State.Class (class MonadState)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Timer (setTimeout)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Audio.WebAudio.Types (AudioContext)
import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Metronome.Audio (BeatMap, loadBeatBuffers, playBeat)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import FRP.Behavior (animate)
import FRP.Behavior.Time (seconds)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Map.Internal (empty)
import Data.Int (fromString)
import Graphics.Drawing (render) as Drawing
import Metronome.Drawing (markers, metronome)
import Metronome.Beat (Bpm, toBeats)

type State =
  { mAudioContext :: Maybe AudioContext
  , mGraphicsContext :: Maybe Context2D
  , beatMap :: BeatMap
  , bpm :: Bpm
  , skew :: Number
  , isRunning :: Boolean
  , runningMetronome :: Effect Unit
  }

data Query a =
    Init a
  | Start a
  | Stop a
  | ChangeTempo Bpm a
  | ChangeSkew Number a

component ::  H.Component HH.HTML Query Unit Void Aff
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { mAudioContext : Nothing
    , mGraphicsContext : Nothing
    , beatMap : empty
    , bpm : 120
    , skew : 0.0
    , isRunning : false
    , runningMetronome : mempty
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Polska Metronome" ]
      , HH.canvas
         [ HP.id_ "canvas"
         , HP.height 350
         , HP.width  800
         ]
      , HH.div
         [HP.id_ "instruction-group" ]
         [ renderStopStart state
         , renderSkewSlider state
         , renderTempoSlider state
         , renderBpm state
         ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval (Init next) = do
    audioCtx <- H.liftEffect newAudioContext
    beatMap <-  H.liftAff $ loadBeatBuffers audioCtx "assets/audio" ["hightom.mp3", "tom.mp3", "hihat.mp3"]
    _ <- H.modify (\st -> st { mAudioContext = Just audioCtx
                             , beatMap = beatMap })
    eval (Start next)
  eval (Start next) = do
    state <- H.get
    mCanvas <- H.liftEffect $ getCanvasElementById "canvas"
    let
      canvas = unsafePartial (fromJust mCanvas)
      audioCtx = unsafePartial (fromJust state.mAudioContext)
    graphicsCtx <- H.liftEffect  $ getContext2D canvas
    runningMetronome <- H.liftEffect  $ animate (toBeats state.skew state.bpm seconds) \beat -> do
         _ <- Drawing.render graphicsCtx (metronome state.skew state.bpm beat)
         playBeat audioCtx state.bpm state.skew state.beatMap beat
    _ <- H.modify (\st -> st { mGraphicsContext = Just graphicsCtx
                             , isRunning = true
                             , runningMetronome = runningMetronome })
    pure next
  eval (Stop next) = do
    _ <- stopAnimation
    pure next
  eval (ChangeTempo bpm next) = do
    state <- H.get
    _ <- stopAnimation
    _ <- H.modify (\st -> st { bpm = bpm })
    pure next
  eval (ChangeSkew skew next) = do
    state <- H.get
    _ <- stopAnimation
    _ <- H.modify (\st -> st { skew = skew })
    pure next

-- rendering functions
renderStopStart :: State -> H.ComponentHTML Query
renderStopStart state =
  let
    label =
      if state.isRunning
        then "Stop"
        else "Start"
    command =
      if state.isRunning
        then Stop
        else Start
  in
    HH.div
      [ HP.class_ (H.ClassName "instruction-component")]
      [ HH.button
        [ HE.onClick (HE.input_ command)
        , HP.class_ $ ClassName "hoverable"
        ]
        [ HH.text label ]
      ]

renderTempoSlider :: State ->  H.ComponentHTML Query
renderTempoSlider state =
  let
     -- | get the value from the slider result, defaulting to 120
    toTempo :: String -> Int
    toTempo s =
      fromMaybe 120 $ fromString s
  in
    HH.div
      [ HP.class_ (H.ClassName "instruction-component")]
      [ HH.label
         [ HP.class_ (H.ClassName "sliderLabel") ]
         [ HH.text "change tempo:" ]

      , HH.input
          [ HE.onValueInput (HE.input ChangeTempo <<< toTempo)
          , HP.type_ HP.InputRange
          , HP.id_ "tempo-slider"
          , HP.min 60.0
          , HP.max 200.0
          , HP.value (show state.bpm)
          ]
      ]

renderSkewSlider :: State ->  H.ComponentHTML Query
renderSkewSlider state =
  let
     -- | get the value from the slider result, defaulting to 0
    toSkew :: String -> Number
    toSkew s =
      (readFloat s) / 100.0
  in
    HH.div
      [ HP.class_ (H.ClassName "instruction-component")]
      [ HH.label
         [ HP.class_ (H.ClassName "sliderLabel") ]
         [ HH.text "shorten 2nd beat:" ]

      , HH.input
          [ HE.onValueInput (HE.input ChangeSkew <<< toSkew)
          , HP.type_ HP.InputRange
          , HP.id_ "skew-slider"
          , HP.min 0.0
          , HP.max 50.0
          , HP.value (show (state.skew * 100.0))
          ]
      ]

renderBpm :: State -> H.ComponentHTML Query
renderBpm state =
  HH.div
    [ HP.class_ (H.ClassName "instruction-component")]
    [ HH.text (show state.bpm <> " bpm") ]


stopAnimation :: ∀ m.
  Bind m =>
  MonadState State m =>
  MonadAff m =>
  m Unit
stopAnimation = do
  state <- H.get
  let
    graphicsCtx = unsafePartial (fromJust state.mGraphicsContext)
  -- stop the metronome immediately
  _ <- H.liftEffect $ setTimeout 0 state.runningMetronome
  -- repaint the static markers (with no moving ball) at the appropriate skew
  _ <- H.liftEffect $ Drawing.render graphicsCtx (markers state.skew)
  -- save state
  _ <- H.modify (\st -> st { isRunning = false
                           , runningMetronome = mempty :: Effect Unit })
  pure unit
