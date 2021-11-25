{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "physics"
, dependencies =
    [ "prelude"
    , "signal"
    , "console"
    , "maybe"  
    , "partial"
    , "canvas"
    , "effect"
    , "psci-support"
    , "react-basic"
    , "js-timers"
    , "math"
    , "web-html"
    , "integers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
