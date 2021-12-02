{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "optparse"
  , "partial"
  , "prelude"
  , "psci-support"
  , "spec"
  , "string-parsers"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
