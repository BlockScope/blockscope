let defs = ./defaults.dhall

in  let deps =
          [ "base"
          , "blaze-html"
          , "containers"
          , "skylighting-core"
          , "skylighting"
          , "filepath"
          , "hakyll"
          , "pandoc >=2.9"
          ]

    in  let exts = ./default-extensions.dhall

        in    defs
            ⫽ exts
            ⫽ { name = "camp-hog"
              , version = "1.0.0"
              , synopsis = "blockscope.com"
              , description = "Static site generator for blockscope.com"
              , category = "web"
              , github = "blockscope/blockscope"
              , homepage =
                  "https://github.com/blockscope/blockscope/tree/develop#readme"
              , executables.site
                =
                { dependencies = deps
                , source-dirs = "www-hakyll"
                , main = "site.hs"
                }
              , tests.hlint
                =
                { dependencies = deps # [ "hlint" ]
                , main = "HLint.hs"
                , source-dirs = [ ".", "test-suite-hlint" ]
                }
              }
