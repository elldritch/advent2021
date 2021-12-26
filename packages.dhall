let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211116/packages.dhall
        sha256:7ba810597a275e43c83411d2ab0d4b3c54d0b551436f4b1632e9ff3eb62e327a

in  upstream
  with sequences =
      { repo = "https://github.com/hdgarrood/purescript-sequences.git"
      , version = "v3.0.2"
      , dependencies =
        [ "newtype"
        , "effect"
        , "console"
        , "profunctor"
        , "arrays"
        , "maybe"
        , "unfoldable"
        , "lazy"
        , "prelude"
        , "nonempty"
        , "assert"
        , "unsafe-coerce"
        , "psci-support"
        , "tuples"
        , "partial"
        ]
      }
