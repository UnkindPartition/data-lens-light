{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables,
             ExistentialQuantification #-}
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
import qualified Data.HashMap.Strict as HM
import Unsafe.Coerce

type Handler req s = forall a . req a -> s -> (s, a)

data UntypedHandler s = forall req . UntypedHandler (Handler req s)

retypeHandler :: UntypedHandler s -> Handler req s
retypeHandler (UntypedHandler h) = unsafeCoerce h

data MultiHandler s all rest = MultiHandler
  (HM.HashMap TypeRep (UntypedHandler s))

instance Category (MultiHandler s) where
  id = MultiHandler HM.empty
  MultiHandler hs1 . MultiHandler hs2 = MultiHandler (hs1 `HM.union` hs2)

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
    loop :: s -> VE a all -> Eff rest (s, a)
    loop s ve =
      case ve of
        Val x -> return (s, x)
        E u@(Union (req :: t (VE a all))) ->
          case
            HM.lookup
              (typeRep (Proxy :: Proxy t))
              hs
          of
            Just uh -> uncurry loop $ retypeHandler uh req s
            Nothing -> send (<$> unsafeReUnion u) >>= loop s
