{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "polska-metronome"
, dependencies =
  [ "affjax"
  , "behaviors"
  , "console"
  , "css"
  , "drawing"
  , "effect"
  , "halogen"
  , "prelude"
  , "psci-support"
  , "webaudio"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
