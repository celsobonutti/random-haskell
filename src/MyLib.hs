module MyLib where

import qualified Data.Map as Map
import Data.Bifunctor (bimap)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)

main :: IO ()
main = putStrLn "Test suite not yet implemented."

indexBy :: (Foldable f, Ord k) => (a -> k) -> f a -> Map k a
indexBy f = foldr (\x y -> uncurry3 Map.insert . tfirst f $ duper x y) mempty

groupBy :: (Foldable f, Ord k) => (a -> k) -> f a -> Map k (NonEmpty a)
groupBy f = foldr insertOrUpdate mempty
  where
    insertOrUpdate =
       uncurry Map.alter . bimap updateFn f . dupe
    updateFn base =
      \case
        Nothing -> Just (base :| [])
        Just (x :| xs) -> Just (base :| (x : xs))

dupe :: a -> (a, a)
dupe x = (x, x)

duper :: a -> b -> (a, a, b)
duper x y = (x, x, y)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f = \(x, y, z) -> f x y z

class Trifunctor t where
  trimap :: (a -> d) -> (b -> e) -> (c -> f) -> t a b c -> t d e f
  trimap f g h = tthird h . tsecond g . tfirst f

  tfirst :: (a -> d) -> t a b c -> t d b c
  tfirst f = trimap f id id

  tsecond :: (b -> e) -> t a b c -> t a e c
  tsecond g = trimap id g id

  tthird :: (c -> f) -> t a b c -> t a b f
  tthird h = trimap id id h

  {-#MINIMAL trimap | tfirst, tsecond, tthird #-}

instance Trifunctor (,,) where
  trimap f g h (x, y, z) = (f x, g y, h z)
