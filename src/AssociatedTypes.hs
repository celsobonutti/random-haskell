module AssociatedTypes where

import Prelude hiding (Functor, Applicative, Monad, pure, (>>=), (<*>), fmap, (<$>))

type family Wrap f b
type family Unwrap f

class Functor f where
  fmap :: (Unwrap f -> b) -> f -> Wrap f b
  fmap = (<$>)

  (<$>) :: (Unwrap f -> b) -> f -> Wrap f b
  (<$>) = fmap
  {-# MINIMAL fmap | (<$>) #-}

class Functor t => Applicative t where
  pure :: (Unwrap t) -> t

  (<*>) :: (Wrap t (Unwrap t -> b)) -> t -> Wrap t b

class Applicative m => Monad m where
  return :: (Unwrap m) -> m
  return = pure

  (>>=) :: m -> (Unwrap m -> Wrap m b) -> Wrap m b
  {-# MINIMAL (>>=) #-}

-- Functor/Applicative/Monad for Maybe

type instance Wrap (Maybe a) b = Maybe b
type instance Unwrap (Maybe a) = a

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

-- Functor/Applicative/Monad for lists

type instance Wrap [a] b = [b]
type instance Unwrap [a] = a

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

-- Functor/Applicative for (a -> b)

type instance Unwrap ((->) a b) = b
type instance Wrap ((->) a b) c = (a -> c)

instance Functor (a -> b) where
  fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)

instance Applicative (a -> b) where
  pure = const

  (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  (<*>) f g = \x -> f x (g x)

type family UnwrapFirst f
type family UnwrapSecond f
type family BiWrap f a b

class Bifunctor f where
  bimap :: (UnwrapFirst f -> c) -> (UnwrapSecond f -> d) -> f -> BiWrap f c d

  first :: (UnwrapFirst f -> c) -> f -> BiWrap f c (UnwrapSecond f)
  first f = bimap f id

  second :: (UnwrapSecond f -> d) -> f -> BiWrap f (UnwrapFirst f) d
  second g = bimap id g

  {-# MINIMAL bimap #-}

-- Bifunctor for Either

type instance UnwrapFirst (Either a b) = a
type instance UnwrapSecond (Either a b) = b
type instance BiWrap (Either a b) c d = Either c d

instance Bifunctor (Either a b) where
  bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
  bimap f g = \case
    Left x -> Left $ f x
    Right y -> Right $ g y

-- Bifunctor for tuple

type instance UnwrapFirst (a, b) = a
type instance UnwrapSecond (a, b) = b
type instance BiWrap (a, b) c d = (c, d)

instance Bifunctor (a, b) where
  bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
  bimap f g (x, y) = (f x, g y)
