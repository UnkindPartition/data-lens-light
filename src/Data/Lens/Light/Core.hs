module Data.Lens.Light.Core
  ( Lens(..)
  , lens
  , iso
  , getL
  , setL
  , modL
  , modL'
  , (^.)
  )
  where

import Prelude hiding (id, (.))
import Control.Category

-- | Simple lens data type
newtype Lens a b = Lens { runLens :: a -> (b -> a, b) }

instance Category Lens where
  id = iso id id
  x . y =
    lens
      (getL x . getL y)
      (\b -> modL y $ setL x b)

-- | Build a lens out of a getter and setter
lens :: (a -> b) -> (b -> a -> a) -> Lens a b
lens get set = Lens $ \a -> (flip set a, get a)

-- | Build a lens out of an isomorphism
iso :: (a -> b) -> (b -> a) -> Lens a b
iso f g = lens f (\x _ -> g x)

-- | Get the getter function from a lens
getL :: Lens a b -> a -> b
getL l = snd . runLens l

-- | Get the setter function from a lens
setL :: Lens a b -> b -> a -> a
setL l = flip $ fst . runLens l

-- | Get the modifier function from a lens
modL :: Lens a b -> (b -> b) -> a -> a
modL l f a =
  case runLens l a of
    (setx, x) -> setx (f x)

-- | Get the modifier function from a lens. Forces function application.
modL' :: Lens a b -> (b -> b) -> a -> a
modL' l f a =
  case runLens l a of
    (setx, x) -> setx $! f x

-- | Infix version of 'getL' (with the reverse order of the arguments)
infixl 9 ^.
(^.) :: b -> Lens b c -> c
(^.) = flip getL
