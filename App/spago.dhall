{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "halogen"
  , "halogen-store"
  , "halogen-vdom"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
