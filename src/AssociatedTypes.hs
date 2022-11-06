module AssociatedTypes where

import Prelude hiding (Functor, Applicative, Monad, pure, (>>=), (<*>), fmap)

type family Wrap f b
type family Unwrap f

type instance Wrap (Maybe a) b = Maybe b
type instance Unwrap (Maybe a) = a

type instance Wrap [a] b = [b]
type instance Unwrap [a] = a

class Functor f where
  fmap :: (Unwrap f -> b) -> f -> Wrap f b

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

class Functor t => Applicative t where
  pure :: (Unwrap t) -> t
  (<*>) :: (Wrap t (Unwrap t -> b)) -> t -> Wrap t b

class Applicative m => Monad m where
  return :: (Unwrap m) -> m
  return = pure

  (>>=) :: m -> (Unwrap m -> Wrap m b) -> Wrap m b
  {-# MINIMAL (>>=) #-}

class Bifunctor f where
  type UnwrapFst f
  type UnwrapSnd f
  type BiWrap f c d

  bimap :: (UnwrapFst f -> c) -> (UnwrapSnd f -> d) -> f -> BiWrap f c d

instance Functor (Maybe a)  where
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
  type UnwrapFst (Either a _) = a
  type UnwrapSnd (Either _ b) = b
  type BiWrap _ c d = Either c d

  bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
  bimap f g = \case
    Left x -> Left $ f x
    Right y -> Right $ g y
