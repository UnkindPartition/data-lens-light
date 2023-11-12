let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/main/haskell-ci.dhall

in    haskellCi.generalCi
        haskellCi.defaultCabalSteps
        ( haskellCi.DhallMatrix::{=}
          with ghc =
            [ haskellCi.GHC.GHC963
            , haskellCi.GHC.GHC947
            , haskellCi.GHC.GHC928
            , haskellCi.GHC.GHC902
            , haskellCi.GHC.GHC8107
            , haskellCi.GHC.GHC884
            , haskellCi.GHC.GHC865
            ]
        )
    : haskellCi.CI.Type
