{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "polska-metronome"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "arrays"
  , "canvas"
  , "colors"
  , "datetime"
  , "drawing"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "http-methods"
  , "hyrule"
  , "integers"
  , "js-timers"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "prelude"
  , "transformers"
  , "tuples"
  , "webaudio"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
