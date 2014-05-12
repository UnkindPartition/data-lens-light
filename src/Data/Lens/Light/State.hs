{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances #-}
module Data.Lens.Light.State
  ( access
  , (~=)
  , (!=)
  , (%=)
  , (!%=)
  , zoom
  , MonadStateT
  )
  where

import Control.Monad.State.Class
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.State as Lazy
import Control.Monad.Trans
import Data.Lens.Light.Core

-- | Get the value of a lens into state
access :: MonadState a m => Lens a b -> m b
access l = gets (getL l)

-- | Set a value using a lens into state
(~=) :: MonadState a m => Lens a b -> b -> m ()
l ~= b = modify $ setL l b

-- | Set a value using a lens into state. Forces both the value and the
-- whole state.
(!=) :: MonadState a m => Lens a b -> b -> m ()
l != b = modify' $ setL l $! b

#if !MIN_VERSION_mtl(2,2,0)
-- Copied from mtl-2.2.0.1
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = state (\s -> let s' = f s in s' `seq` ((), s'))
#endif

infixr 4 ~=, !=

-- | Infix modification of a value through a lens into state
(%=) :: MonadState a m => Lens a b -> (b -> b) -> m ()
l %= f = modify $ modL l f

-- | Infix modification of a value through a lens into state. Forces both
-- the function application and the whole state.
(!%=) :: MonadState a m => Lens a b -> (b -> b) -> m ()
l !%= f = modify' $ modL' l f

infixr 4 %=, !%=

-- | The purpose of this class is to abstract the difference between the
-- lazy and strict state monads, so that 'zoom' can work with either of
-- them.
class MonadStateT t where
  runStateT :: t s m a -> s -> m (a, s)

instance MonadStateT Strict.StateT where runStateT = Strict.runStateT
instance MonadStateT Lazy.StateT where runStateT = Lazy.runStateT

-- | Run a stateful computation with a smaller state inside another
-- computation with a bigger state.
zoom
  :: ( MonadStateT stateT
     , MonadState s (stateT s m)
     , MonadTrans (stateT s)
     , Monad m
     )
  => Lens s s'
  -> stateT s' m a
  -> stateT s m a
zoom l a = do
  s <- get
  (r, s') <- lift $ runStateT a (s ^. l)
  modify' $ setL l s'
  return r
