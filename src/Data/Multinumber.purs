
-- | This module defines a type of multisets called `multinumbers`, see
-- |
-- | G.P.Monro — THE CONCEPT OF MULTISET ;
-- | Zeitscrh. f. math. Logik und Grundlagen d. Math. Bd. 33, S. 171-178 (1987)
-- |

module Data.Multinumber
  ( Multinumber()
  , empty
  , isEmpty
  , singleton
  , insert
  , member
  , delete
  , fromFoldable
  , toList
  , fromList
  , toArray
  , size
  , union
  , unions
  , difference
  , subset
  , properSubset
  , intersection
  ) where

import Prelude (
  Eq, (==), (/=), (<),
  Ord, compare,
  Show, show,
  Semigroup, append,
  otherwise, map, flip,
  ($), (<<<), (++), (&&))

import Prelude

import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Array as A

import Data.Tuple (snd)
import Data.Monoid (Monoid, mempty)
import Data.Foldable (Foldable, foldMap, foldl, foldr, sum)

import Control.Monad.Eff (runPure, Eff())
import Control.Monad.ST (ST())
import Control.Monad.Rec.Class (tailRecM2)
import Data.Array.ST
import Data.Array.Unsafe (unsafeIndex)
import Data.Either
import Data.Maybe

-- | `Set a` represents a set of values of type `a`
data Multinumber a = Multinumber (M.Map a (L.List a))

instance eqMSet :: (Eq a) => Eq (Multinumber a) where
  eq (Multinumber m1) (Multinumber m2) = m1 == m2

instance showMSet :: (Show a) => Show (Multinumber a) where
  show m = "fromList " ++ show (toList m)

instance ordSet :: (Ord a) => Ord (Multinumber a) where
  compare m1 m2 = compare (toList m1) (toList m2)

-- instance monoidSet :: (Ord a) => Monoid (Multinumber a) where
--   mempty = empty
--
-- instance semigroupSet :: (Ord a) => Semigroup (Multinumber a) where
--   append = union
--
-- instance foldableSet :: Foldable Set where
--   foldMap f = foldMap f <<< toList
--   foldl f x = foldl f x <<< toList
--   foldr f x = foldr f x <<< toList

-- | Create a set with one element
singleton :: forall a. a -> Multinumber a
singleton x = Multinumber (M.singleton x $ L.singleton x)

-- | An empty Multinumber
empty :: forall a. Multinumber a
empty = Multinumber M.empty

-- | Test if a set is empty
isEmpty :: forall a. Multinumber a -> Boolean
isEmpty (Multinumber m) = M.isEmpty m

-- | Insert a value into a set
insert :: forall a. (Ord a) => a -> Multinumber a -> Multinumber a
insert x (Multinumber m) = Multinumber (M.alter f x m)
  where
    f Nothing = Just (L.singleton x)
    f (Just xs) = Just (L.Cons x xs)

-- | Delete a value from a set
delete :: forall a. (Ord a) => a -> Multinumber a -> Multinumber a
delete x (Multinumber m) = Multinumber (M.alter f x m)
  where
    f Nothing = Nothing
    f (Just L.Nil) = Nothing
    f (Just (L.Cons e xs)) | L.length xs == 0 = Nothing
                           | otherwise = Just xs

-- | Test if a value is a member of a set
member :: forall a. (Ord a) => a -> Multinumber a -> Boolean
member x (Multinumber m) = x `M.member` m

-- | Convert a Multinumber to a list
toList :: forall a. Multinumber a -> L.List a
toList (Multinumber m) = L.concatMap snd (M.toList m)

-- | Convert a Multinumber into an array
toArray :: forall a. (Ord a) => Multinumber a -> Array a
toArray = L.fromList <<< toList

-- | Form the union of two sets
-- |
-- | Running time: `O(n * log(m))`
union :: forall a. (Ord a) => Multinumber a -> Multinumber a -> Multinumber a
union (Multinumber m1) (Multinumber m2) = Multinumber (M.unionWith concat2 m1 m2)
  where concat2 l1 l2 = L.concat $ L.fromFoldable [ l1, l2 ]

-- | Create a set from a foldable collection of elements
fromFoldable :: forall f a. (Foldable f, Ord a) => f a -> Multinumber a
fromFoldable = foldl (\m a -> insert a m) empty

-- | Create a set from a list of elements
fromList :: forall a. (Ord a) => L.List a -> Multinumber a
fromList = fromFoldable

-- | Find the size of a set
size :: forall a. Multinumber a -> Int
size (Multinumber m) = sum $ map (L.length <<< snd) (M.toList m)

-- | Form the union of a collection of sets
unions :: forall a. (Ord a) => L.List (Multinumber a) -> Multinumber a
unions = foldl union empty

-- | Form the set difference
difference :: forall a. (Ord a) => Multinumber a -> Multinumber a -> Multinumber a
difference s1 s2 = foldl (flip delete) s1 (toList s2)

-- | True if and only if every element in the first set
-- | is an element of the second set
subset :: forall a. (Ord a) => Multinumber a -> Multinumber a -> Boolean
subset s1 s2 = isEmpty $ s1 `difference` s2

-- | True if and only if the first set is a subset of the second set
-- | and the sets are not equal
properSubset :: forall a. (Ord a) => Multinumber a -> Multinumber a -> Boolean
properSubset s1 s2 = subset s1 s2 && (s1 /= s2)

-- | The set of elements which are in both the first and second set
intersection :: forall a. (Ord a) => Multinumber a -> Multinumber a -> Multinumber a
intersection s1 s2 = fromFoldable $ runPure (runSTArray (emptySTArray >>= intersect)) where
  ls = toArray s1
  rs = toArray s2
  ll = A.length ls
  rl = A.length rs
  intersect :: forall h r. STArray h a -> Eff (st :: ST h | r) (STArray h a)
  intersect acc = tailRecM2 go 0 0 where
    go l r =
      if l < ll && r < rl
      then case compare (ls `unsafeIndex` l) (rs `unsafeIndex` r) of
        EQ -> do
          pushSTArray acc (ls `unsafeIndex` l)
          pure $ Left {a: l + 1, b: r + 1}
        LT -> pure $ Left {a: l + 1, b: r}
        GT -> pure $ Left {a: l, b: r + 1}
      else pure $ Right acc
