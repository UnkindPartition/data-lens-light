{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables,
             ExistentialQuantification, KindSignatures, CPP #-}
module Data.Lens.Light.MultiRWS
  ( MultiHandler
  , stateHandler
  , runMultiRWS
  ) where
import Control.Category
import Control.Eff
import Control.Eff.State.Lazy
import Control.Applicative
import Data.Lens.Light.Core
import Data.Typeable
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Unsafe.Coerce

type Handler req s = forall a . req a -> s -> (s, a)

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
stateHandler' l (State t k) s =
  let
    ls  = getL l s
    ls' = t ls
    s'  = setL l ls' s
  in (s', k ls')

stateHandler
  :: forall a s r . Typeable a
  => Lens s a -> MultiHandler s (State a :> r) r
stateHandler l =
  MultiHandler $
    HM.singleton
      (typeRep (Proxy :: Proxy (State a)))
      (UntypedHandler $ stateHandler' l)

runMultiRWS
  :: forall s all rest a
  .  MultiHandler s all rest
  -> s
  -> Eff all a
  -> Eff rest (s, a)
runMultiRWS (MultiHandler hs) s0 action = loop s0 (admin action)
  where
    loop :: s -> VE all a -> Eff rest (s, a)
    loop s ve =
      case ve of
        Val x -> return (s, x)
        E u@(Union (req :: t (VE all a))) ->
          case
            HM.lookup
              (typeRep (Proxy :: Proxy t))
              hs
          of
            Just uh -> uncurry loop $ retypeHandler uh req s
            Nothing -> send (<$> unsafeReUnion u) >>= loop s

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
