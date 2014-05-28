{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables,
             ExistentialQuantification, KindSignatures, CPP,
             FlexibleContexts #-}
module Data.Lens.Light.MultiRWS
  ( MultiHandler
  , stateHandler
  , readerHandler
  , writerHandler
  , runMultiRWS
  ) where
import Prelude hiding ((.), id)
import Control.Category
import Control.Eff
import Control.Eff.State
import Control.Eff.Reader
import Control.Eff.Writer
import Control.Applicative
import Control.Monad
import Data.Lens.Light.Core
import Data.Typeable
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Unsafe.Coerce

type Handler req s = forall a . req a -> State s a

data UntypedHandler s = forall req . UntypedHandler (Handler req s)

retypeHandler :: UntypedHandler s -> Handler req s
retypeHandler (UntypedHandler h) = unsafeCoerce h

newtype MultiHandler s all rest = MultiHandler
  (HM.HashMap TypeRep (UntypedHandler s))

instance Category (MultiHandler s) where
  id = MultiHandler HM.empty
  MultiHandler hs1 . MultiHandler hs2 = MultiHandler $
    case hs1 `safeUnion` hs2 of
      Left ty -> error $
        "Data.Lens.Light: multiple lenses for the same request type (" ++ show ty ++ ")"
      Right u -> u

stateHandler' :: Lens s a -> Handler (State a) s
stateHandler' l (State f) =
  let
    f' s =
      case f (s ^. l) of
        (s', r) -> (setL l s' s, r)
  in
    State f'

stateHandler
  :: forall a s r . Typeable a
  => Lens s a -> MultiHandler s (State a :> r) r
stateHandler l =
  MultiHandler $
    HM.singleton
      (typeRep (Proxy :: Proxy (State a)))
      (UntypedHandler $ stateHandler' l)

readerHandler' :: Lens s a -> Handler (Reader a) s
readerHandler' l (Reader k) = State (\s -> (s, k $ getL l s))

readerHandler
  :: forall a s r . Typeable a
  => Lens s a -> MultiHandler s (Reader a :> r) r
readerHandler l =
  MultiHandler $
    HM.singleton
      (typeRep (Proxy :: Proxy (Reader a)))
      (UntypedHandler $ readerHandler' l)

writerHandler' :: Monoid a => Lens s a -> Handler (Writer a) s
writerHandler' l (Writer w v) =
  State (\s -> (modL l (`mappend` w) s, v))

writerHandler
  :: forall a s r . (Typeable a, Monoid a)
  => Lens s a -> MultiHandler s (Writer a :> r) r
writerHandler l =
  MultiHandler $
    HM.singleton
      (typeRep (Proxy :: Proxy (Writer a)))
      (UntypedHandler $ writerHandler' l)

runMultiRWS
  :: forall s all rest a . (Typeable s, Member (State s) rest)
  => MultiHandler s all rest
  -> Eff all a
  -> Eff rest a
runMultiRWS (MultiHandler hs) action =
  runEff action
    (\x -> return x)
    (\u@(Union (req :: t (Eff rest a))) -> join $
      case
        HM.lookup
          (typeRep (Proxy :: Proxy t))
          hs
      of
        Just uh -> case retypeHandler uh req of State f -> state f
        Nothing -> sendU (<$> unsafeReUnion u)
    )

-- | Union that returns an error when duplicate entries are found.
--
-- This is probably less efficient than 'HM.union', but this is performed
-- only once per each lens, so we don't care.
--
-- Less inefficient for @smallMap `safeUnion` bigMap@, which is consistent
-- with the right associativity of '.'.
safeUnion
  :: (Eq k, Hashable k)
  => HM.HashMap k v
  -> HM.HashMap k v
  -> Either k (HM.HashMap k v)
safeUnion m1 m2 =
  HM.foldrWithKey
    (\k v kont m ->
      if isJust (HM.lookup k m)
        then Left k
        else kont $ HM.insert k v m
    ) Right m1 $ m2

#if !MIN_VERSION_base(4,7,0)
data Proxy (a :: * -> *) = Proxy

typeRep :: forall f . Typeable1 f => Proxy f -> TypeRep
typeRep _ = typeOf1 (undefined :: f ())

#endif
