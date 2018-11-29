    let defs = ./defaults.dhall

in    defs
    ⫽ { name =
          "camp-hog"
      , version =
          "1.0.0"
      , synopsis =
          "blockscope.com"
      , description =
          "Static site generator for blockscope.com"
      , category =
          "web"
      , github =
          "blockscope/blockscope"
      , homepage =
          "https://github.com/blockscope/blockscope/tree/develop#readme"
      , executables =
          { site =
              { dependencies =
                  [ "base", "hakyll", "pandoc", "blaze-html" ]
              , source-dirs =
                  "CampHog"
              , main =
                  "site.hs"
              }
          }
      , tests =
          { hlint =
              { dependencies =
                  [ "base", "hakyll", "pandoc", "blaze-html", "hlint" ]
              , main =
                  "HLint.hs"
              , source-dirs =
                  [ ".", "test-suite-hlint" ]
              }
          }
      }
