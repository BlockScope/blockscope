{ version =
    "0.1.0"
, author =
    "Phil de Joux"
, maintainer =
    "phil.dejoux@blockscope.com"
, copyright =
    "\u00A9 2017-2018 Phil de Joux, \u00A9 2017-2018 Block Scope Limited"
, license =
    "MPL-2.0"
, license-file =
    "LICENSE.md"
, tested-with =
    "GHC == 8.2.2"
, extra-source-files =
    [ "package.dhall", "package.yaml", "changelog.md", "README.md" ]
, ghc-options =
    [ "-Wall", "-Werror" ]
, default-extensions =
    [ "PackageImports" ]
, dependencies =
    [ "base >=4.10.1.0 && <5" ]
}
