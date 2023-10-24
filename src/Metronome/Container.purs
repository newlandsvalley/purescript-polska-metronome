module Metronome.Container where

import Prelude

import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Audio.WebAudio.Types (AudioContext)
import Control.Monad.State.Class (class MonadState)
import Data.Int (fromString, round, toNumber)
import Data.Map (empty)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Number (fromString) as Num
import Data.Ord (abs)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Timer (setTimeout)
import FRP.Behavior (animate)
import FRP.Behavior.Time (seconds)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Graphics.Drawing (render) as Drawing
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..), HTML)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Metronome.Audio (BeatMap, loadBeatBuffers, playBeat)
import Metronome.Beat (Beat(..), BeatNumber(..), Bpm, PolskaType(..), toBeats)
import Metronome.Drawing (canvasHeight, canvasWidth, markers, metronome)
import Partial.Unsafe (unsafePartial)
import Web.HTML (window) as HTML
import Web.HTML.Window (Window, innerWidth, fromEventTarget, toEventTarget) as Window
import Web.Event.Event (EventType(..), target) as Event

type Slot = H.Slot Query Void

type State =
  { mAudioContext :: Maybe AudioContext
  , mGraphicsContext :: Maybe Context2D
  , polskaType :: PolskaType
  , beatMap :: BeatMap
  , bpm :: Bpm
  , skew :: Number
  , scale :: Number  -- the scale of the animated drawing
  , silentBeatOne :: Boolean
  , isRunning :: Boolean
  , runningMetronome :: Effect Unit
  , windowWidth :: Int
  , mWindowSubscriptionId :: Maybe H.SubscriptionId
  }

data Action =
    Init
  | Start
  | Stop
  | ChangeTempo Bpm
  | ChangeSkew Number
  | ChangePolskaType String
  | MakeBeatTwoSilent Boolean
  | HandleWindowResize (Maybe Window.Window)
  | Finalize

-- the only reason that we need Query at all is that we need to chain
-- the playing of the metronome after initialisation or after button settings
-- change and this is not possible just with actions - we need a query
--
-- Rendering takes place between the two initialisations.
data Query a =
  StartMetronome a

component :: ∀ i o m. MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Init
        , finalize = Just Stop
        }
    }

  where
  initialState :: i -> State
  initialState _ =
    { mAudioContext : Nothing
    , mGraphicsContext : Nothing
    , polskaType : ShortFirst
    , beatMap : empty
    , bpm : 120
    , skew : 0.25
    , scale : 0.90
    , silentBeatOne : false
    , isRunning : false
    , runningMetronome : mempty
    , windowWidth : 0
    , mWindowSubscriptionId : Nothing
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Polska Metronome" ]
      , HH.div
        [ HP.class_ (H.ClassName "metronome-center") 
        , HP.id "metronome-controls" 
        ]
        [ 
          renderStopStart state
        , renderSilentBeatTwo state
        , renderPolskaTypeMenu state
        , renderTempoSlider state
        , renderSkewSlider state
        -- debug only 
        -- , renderWindowWidth state
        ]
      , HH.div
        [ HP.class_ (H.ClassName "metronome-center") 
        , HP.id "metronome-display" 
        ]
        [ HH.canvas
            [ HP.id "canvas"
            , HP.height (scaleDimension canvasHeight state.scale)
            , HP.width  (scaleDimension canvasWidth  state.scale)
            ]
        ]
      ]

  handleAction ∷ Action → H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Init -> do
      audioCtx <- H.liftEffect newAudioContext
      beatMap <-  H.liftAff $ loadBeatBuffers audioCtx "assets/audio" ["hightom.mp3", "tom.mp3", "hihat.mp3"]
      -- subscribe to window resize events so that we can dynamically change the moving graphics display size
      window <- H.liftEffect HTML.window
      subscriptionId <- H.subscribe do
        eventListener
          (Event.EventType "resize")
          (Window.toEventTarget window)
          (Event.target >>> map (Window.fromEventTarget >>> HandleWindowResize))
      windowWidth <- H.liftEffect $ Window.innerWidth window

      let 
        scale = scaleDisplayToWindow windowWidth

      _ <- H.modify (\st -> st { mAudioContext = Just audioCtx
                               , beatMap = beatMap 
                               , scale = scale
                               , windowWidth = windowWidth
                               , mWindowSubscriptionId = Just subscriptionId})
      _ <- handleQuery (StartMetronome unit)
      pure unit
    Start -> do
      -- defer to the query
      _ <- handleQuery (StartMetronome unit)
      pure unit
    Stop -> do
      _ <- stopAnimation
      pure unit
    ChangeTempo bpm -> do
      _ <- stopAnimation
      _ <- H.modify (\st -> st { bpm = bpm })
      pure unit
    ChangeSkew skew -> do
      _ <- stopAnimation
      _ <- H.modify (\st -> st { skew = skew })
      _ <- handleQuery (StartMetronome unit)
      pure unit
    ChangePolskaType s -> do
      state <- H.get
      let
        polskaType = readPolskaType s state.polskaType
        skew = recalculateSkew state polskaType
      _ <- stopAnimation
      _ <- H.modify (\st -> st { polskaType = polskaType, skew = skew })
      _ <- handleQuery (StartMetronome unit)
      pure unit
    MakeBeatTwoSilent makeSilent -> do
      _ <- stopAnimation
      _ <- H.modify (\st -> st { silentBeatOne = makeSilent })
      pure unit
    HandleWindowResize mWindow -> do 
      -- if the browser window changes size significantly, scale up/down the dynamic display accordingly 
      case mWindow of 
        Just window -> do
          state <- H.get
          windowWidth <- H.liftEffect $ Window.innerWidth window
          -- primitive debouncing - only register a change of more than 50 pixels since the last width
          if (abs (windowWidth - state.windowWidth ) > 50) then do
            let 
              scale = scaleDisplayToWindow windowWidth
            _ <- H.modify (\st -> st { scale = scale     
                                     , windowWidth = windowWidth })
            _ <- stopAnimation
            pure unit
          else
            pure unit 
        _ -> 
          pure unit
    Finalize -> do
      state <- H.get
      _ <- stopAnimation
      -- unsubscribe from the window resize
      case state.mWindowSubscriptionId of 
        Just susbscriptionId ->
          H.unsubscribe susbscriptionId
        _ ->
          pure unit

handleQuery :: ∀ o a m. MonadAff m => Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  StartMetronome next -> do
    state <- H.get
    mCanvas <- H.liftEffect $ getCanvasElementById "canvas"
    let
      canvas = unsafePartial (fromJust mCanvas)
      audioCtx = unsafePartial (fromJust state.mAudioContext)
    graphicsCtx <- H.liftEffect  $ getContext2D canvas
    runningMetronome <- H.liftEffect  $ animate (toBeats state.polskaType state.skew state.bpm seconds) \beat -> do
         _ <- Drawing.render graphicsCtx (metronome state.polskaType state.skew state.bpm beat state.scale)
         maybePlayBeat audioCtx state beat
    _ <- H.modify (\st -> st { mGraphicsContext = Just graphicsCtx
                             , isRunning = true
                             , runningMetronome = runningMetronome })
    pure (Just next)

-- we always attempt to play the beat when the moving circle hits a rigid beat marker
-- except for the case where the user opts for a silent second beat (Beat One)
maybePlayBeat :: AudioContext -> State -> Beat -> Effect Unit
maybePlayBeat audioCtx state beat@(Beat { number, proportion }) = 
  if (number == One) && (state.silentBeatOne) then 
    pure unit 
  else
    playBeat audioCtx state.bpm state.skew state.beatMap beat

renderStopStart :: ∀ m. State -> H.ComponentHTML Action () m
renderStopStart state =
  HH.div
    [ HP.class_ (H.ClassName "leftPanelComponent")]
    [ HH.label
      [ HP.class_ (H.ClassName "labelAlignment") ]
      [ HH.text "metronome:" ]
    , renderStopStartButton state 
    ]

-- rendering function

renderStopStartButton :: ∀ m. State -> H.ComponentHTML Action () m
renderStopStartButton state =
  let
    label =
      if state.isRunning
        then "stop"
        else "start"
    command =
      if state.isRunning
        then Stop
        else Start
  in
    HH.button
      [ HE.onClick \_ -> command
      , HP.class_ $ ClassName "hoverable"
      ]
      [ HH.text label ]

renderSilentBeatTwo :: ∀ m. State -> H.ComponentHTML Action () m
renderSilentBeatTwo state =
  let
    soundedState =
      if state.silentBeatOne
        then "off"
        else "on"
  in 
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [ HH.label
        [ HP.class_ (H.ClassName "labelAlignment") ]
        [ HH.text ("beat 2: " <> soundedState) ]
      , renderMakeSilent state 
      ]
      
renderMakeSilent :: ∀ m. State -> H.ComponentHTML Action () m
renderMakeSilent state =
  let
    label =
      if state.silentBeatOne
        then "turn on"
        else "turn off"
    command =
      if state.silentBeatOne
        then (MakeBeatTwoSilent false)
        else (MakeBeatTwoSilent true)
  in
    HH.button
      [ HE.onClick \_ -> command
      , HP.class_ $ ClassName "hoverable"
      ]
      [ HH.text label ]

renderTempoSlider :: ∀ m. State -> H.ComponentHTML Action () m
renderTempoSlider state =
  let
     -- | get the value from the slider result, defaulting to 120
    toTempo :: String -> Int
    toTempo s =
      fromMaybe 120 $ fromString s
  in
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [ HH.label
         [ HP.class_ (H.ClassName "labelAlignment") ]
         [ HH.text "change tempo:" ]

      , HH.input
          [ HE.onValueInput (ChangeTempo <<< toTempo)
          , HP.type_ HP.InputRange
          , HP.id "tempo-slider"
          , HP.min 60.0
          , HP.max 240.0
          , HP.value (show state.bpm)
          ]
      , renderBpm state
      ]

-- | the slider is callibrated as follows-
-- |  short first and finnskogpols:
-- |     Slider Position        skew
-- |         0 (min)             0.5 (max skew)
-- |        50 (max)             0 (no skew)
-- |  long first:
-- |     Slider Position        skew
-- |         0 (min)             0 (no skew)
-- |        50 (max)             -0.5 (max skew)
renderSkewSlider :: ∀ m. State -> H.ComponentHTML Action () m
renderSkewSlider state =
  let
     -- | get the value from the slider result, defaulting to 50 (no skew)
    toSkew :: PolskaType -> String -> Number
    toSkew polskaType s =
      if (polskaType == LongFirst) then
        (0.0 - readNumber s) / 100.0
      else
        (50.0 - readNumber s) / 100.0
    fromSkew :: PolskaType -> Number -> String
    fromSkew polskaType _ =
      if (polskaType == LongFirst) then
        show ((0.0 - state.skew) * 100.0)
      else
        show ((0.5 - state.skew) * 100.0)
  in
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [ HH.label
         [ HP.class_ (H.ClassName "labelAlignment") ]
         [ HH.text "move 2nd marker:" ]

      , HH.input
          [ HE.onValueInput (ChangeSkew <<< toSkew state.polskaType)
          , HP.type_ HP.InputRange
          , HP.id "skew-slider"
          , HP.min 0.0
          , HP.max 50.0
          , HP.value (fromSkew state.polskaType state.skew)
          ]
      , renderSkew state
      ]

renderBpm :: ∀ m. State -> H.ComponentHTML Action () m
renderBpm state =
  HH.div
    [ HP.class_ (H.ClassName "slider-state")]
    [ HH.text (show state.bpm <> " bpm") ]
    
renderSkew :: ∀ m. State -> H.ComponentHTML Action () m
renderSkew state =
  HH.div
    [ HP.class_ (H.ClassName "slider-state")]
    [ HH.text (show (0 - (round $ state.skew * 100.0)) <> "%") ]

{- debug only 
renderWindowWidth :: ∀ m. State -> H.ComponentHTML Action () m
renderWindowWidth state =
  HH.div
    [ HP.class_ (H.ClassName "leftPanelComponent")]
    [ HH.label
      [ HP.class_ (H.ClassName "labelAlignment") ]
      [ HH.text "window width:" ]
    , HH.text (show state.windowWidth)
    ]
-}

-- | a menu option is a string representing the option and a boolean indicating
-- | whether it is selected
data MenuOption =
  MenuOption String Boolean

renderPolskaTypeMenu :: ∀ m. State -> H.ComponentHTML Action () m
renderPolskaTypeMenu state =
    let
      f :: ∀ p i. MenuOption -> HTML p i
      f mo =
        case mo of
          MenuOption text selected ->
            HH.option
              [ HP.selected selected
              , HP.value text ]
              [ HH.text text]
    in
      HH.div
        [ HP.class_ (H.ClassName "leftPanelComponent")]
        [ HH.label
           [ HP.class_ (H.ClassName "labelAlignment") ]
           [ HH.text "polska type:" ]
        , HH.select
            [ HP.class_ $ ClassName "selection"
            , HE.onValueChange (ChangePolskaType)
            , HP.id  "polska-menu"
            , HP.value (show state.polskaType)
            ]
            (map f $ polskaTypeOptions state.polskaType)
        ]

polskaTypeOptions :: PolskaType -> Array MenuOption
polskaTypeOptions polskaType =
  [ MenuOption (show ShortFirst) (polskaType == ShortFirst)
  , MenuOption (show LongFirst) (polskaType == LongFirst)
  , MenuOption (show Finnskogpols) (polskaType == Finnskogpols)
  ]


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
  _ <- H.liftEffect $ Drawing.render graphicsCtx (markers state.polskaType state.skew state.scale)
  -- save state
  _ <- H.modify (\st -> st { isRunning = false
                           , runningMetronome = mempty :: Effect Unit })
  pure unit

readPolskaType :: String -> PolskaType -> PolskaType
readPolskaType s default =
  case s of
    "Finnskogpols" -> Finnskogpols
    "Long first beat" -> LongFirst
    "Short first beat" -> ShortFirst
    _ -> default

-- | recalculate the skew if the polska type changes
-- | if we change to or from a long first beat polska, the sign reverses
-- | otherwise it's unchanged
recalculateSkew :: State -> PolskaType -> Number
recalculateSkew state newPolskaType =
  if (state.polskaType == LongFirst) && (newPolskaType /= LongFirst) then
    0.0 - state.skew
  else if (state.polskaType /= LongFirst) && (newPolskaType == LongFirst) then
    0.0 - state.skew
  else
    state.skew

readNumber :: String -> Number 
readNumber s = 
  fromMaybe 0.0 $ Num.fromString s

scaleDimension :: Int -> Number -> Int 
scaleDimension dimension scale =
  round (scale * (toNumber dimension))

-- fluid scaling of the dynamic metronome display
scaleDisplayToWindow :: Int -> Number
scaleDisplayToWindow windowWidth = 
  let
    largeScreenSize = 1600 
    smallScreenSize = 400 
    largeScreenScale = 1.0
    smallScreenScale = 0.5

  in
    -- mobiles
    if (windowWidth <= smallScreenSize) then 
      smallScreenScale
    -- larger screens
    else if (windowWidth > largeScreenSize) then 
      largeScreenScale 
    -- in between
    else 
      smallScreenScale 
      + ( toNumber (windowWidth - smallScreenSize ) 
        / toNumber (largeScreenSize - smallScreenSize)
        * (largeScreenScale - smallScreenScale)
        )
