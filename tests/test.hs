{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction,
             ScopedTypeVariables, TypeOperators, DeriveDataTypeable #-}
import Prelude hiding ((.), id)
import Control.Category
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import Data.Typeable

import Control.Eff
import qualified Control.Eff.State as SL

import Data.Lens.Light

data Foo = Foo
  { _fooInt :: Int
  , _fooBool :: Bool
  }
  deriving (Show, Eq, Typeable)

data Bar = Bar { _bar1 :: Int, _bar2 :: Int }
  deriving (Show, Eq, Typeable)

makeLenses [''Foo, ''Bar]

fooStateHandler =
  stateHandler fooInt .
  stateHandler fooBool

barStateHandler =
  stateHandler bar1 .
  stateHandler bar2

main = defaultMain $ testGroup "Tests"
  [ lazyStateTests ]

foo :: Foo
foo = Foo 0 False

-- t :: Eff (SL.State Int :> ()) a -> (Int, a)
t a = run $ SL.runState foo $ runMultiRWS fooStateHandler a

lazyStateTests = testGroup "State.Lazy"
  [ testCase "get" $
      t SL.get @?= (foo, False)
  , testCase "put" $
      t (SL.put True) @?= (Foo 0 True, ())
  , testCase "modify" $
      t (SL.modify not) @?= (Foo 0 True, ())
  , testCase "put-modify-get" $
      t (SL.put (7 :: Int) >> SL.modify (*(2 :: Int)) >> SL.get) @?= (Foo 14 False, (14 :: Int))
  , testCase "ambiguity" $ do
      e <- try . evaluate $
        run $ SL.runState (Bar 1 2) $ (runMultiRWS barStateHandler SL.get)
      case e of
        Left (ErrorCall {}) -> return ()
        Right (_, _ :: Int) -> assertFailure "Ambiguity didn't lead to error"
  ]
