let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "example/src/**/*.purs" ],
  dependencies = conf.dependencies # [ "web-dom" ]
}
