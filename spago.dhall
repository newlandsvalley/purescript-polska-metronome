{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "polska-metronome"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "behaviors"
  , "canvas"
  , "colors"
  , "datetime"
  , "drawing"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "http-methods"
  , "integers"
  , "js-timers"
  , "math"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "tuples"
  , "webaudio"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
