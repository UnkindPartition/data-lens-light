{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction,
             ScopedTypeVariables #-}
import Prelude hiding ((.), id)
import Control.Category
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit

import Control.Eff
import qualified Control.Eff.State.Lazy as SL

import Data.Lens.Light

data Foo a = Foo
  { _fooInt :: Int
  , _fooPoly :: a
  }
  deriving (Show, Eq)

makeLens ''Foo

fooStateHandler =
  stateHandler fooInt .
  stateHandler fooPoly

main = defaultMain $ testGroup "Tests"
  [ lazyStateTests ]

foo :: Foo Bool
foo = Foo 0 False

t a = run (runMultiRWS fooStateHandler foo a)

lazyStateTests = testGroup "State.Lazy"
  [ testCase "get monomorphic" $
      t SL.get @?= (foo, 0 :: Int)
  , testCase "put monomorphic" $
      t (SL.put (3 :: Int)) @?= (Foo 3 False, ())
  , testCase "modify monomorphic" $
      t (SL.modify ((+ 7) :: Int -> Int)) @?= (Foo 7 False, ())
  , testCase "get polymorphic" $
      t SL.get @?= (foo, False)
  , testCase "put polymorphic" $
      t (SL.put True) @?= (Foo 0 True, ())
  , testCase "modify polymorphic" $
      t (SL.modify not) @?= (Foo 0 True, ())
  , testCase "put-modify-get" $
      t (SL.put (7 :: Int) >> SL.modify (*(2 :: Int)) >> SL.get) @?= (Foo 14 False, 14 :: Int)
  , testCase "ambiguity" $ do
      e <- try . evaluate $
        run (runMultiRWS fooStateHandler (Foo 1 (2 :: Int)) SL.get)
      case e of
        Left (ErrorCall {}) -> return ()
        Right (_, _ :: Int) -> assertFailure "Ambiguity didn't lead to error"
  ]
