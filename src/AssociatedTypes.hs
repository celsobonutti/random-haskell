module AssociatedTypes where

import Prelude hiding (Functor, Applicative, Monad, pure, (>>=), (<*>), fmap)

class Functor f where
  type Wrapped f b
  type Unwrapped f
  fmap :: (Unwrapped f -> b) -> f -> Wrapped f b

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

class Functor t => Applicative t where
  pure :: (Unwrapped t) -> t
  (<*>) :: (Wrapped t (Unwrapped t -> b)) -> t -> Wrapped t b

class Applicative m => Monad m where
  return :: (Unwrapped m) -> m
  return = pure

  (>>=) :: m -> (Unwrapped m -> Wrapped m b) -> Wrapped m b
  {-# MINIMAL (>>=) #-}

class Bifunctor f where
  type UnwrappedFst f
  type UnwrappedSnd f
  type BiWrapped f c d

  bimap :: (UnwrappedFst f -> c) -> (UnwrappedSnd f -> d) -> f -> BiWrapped f c d

instance Functor (Maybe a)  where
  type Wrapped _ b = Maybe b
  type Unwrapped (Maybe a) = a
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f = \case
    Nothing -> Nothing
    Just x -> Just (f x)

instance Applicative (Maybe a) where
  pure = Just
  Just f <*> Just x = Just $ f x
  _ <*> _ = Nothing

instance Monad (Maybe a) where
  Just x >>= f = f x
  Nothing >>= _ = Nothing

instance Functor [a] where
  type Wrapped _ b = [b]
  type Unwrapped [a] = a

  fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
  fmap f (x:xs) = f x : fmap f xs

instance Applicative [a] where
  pure x = [x]

  [] <*> _ = []
  (x : xs) <*> ys = fmap x ys <> (xs <*> ys)

instance Monad [a] where
  xs >>= f = mconcat (f <$> xs)

instance Bifunctor (Either a b) where
  type UnwrappedFst (Either a _) = a
  type UnwrappedSnd (Either _ b) = b
  type BiWrapped _ c d = Either c d

  bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
  bimap f g = \case
    Left x -> Left $ f x
    Right y -> Right $ g y
