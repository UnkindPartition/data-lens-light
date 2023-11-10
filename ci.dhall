let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/main/haskell-ci.dhall

in    haskellCi.generalCi
        haskellCi.matrixSteps
        ( Some
            { ghc =
              [ haskellCi.GHC.GHC963
              , haskellCi.GHC.GHC947
              , haskellCi.GHC.GHC928
              , haskellCi.GHC.GHC902
              , haskellCi.GHC.GHC8107
              , haskellCi.GHC.GHC884
              , haskellCi.GHC.GHC865
              ]
            , cabal = [ haskellCi.latestCabal ]
            }
        )
    : haskellCi.CI.Type
