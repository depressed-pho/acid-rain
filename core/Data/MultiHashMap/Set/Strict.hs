{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | This is like 'Data.Multimap.Set' from the @multi-containers@
-- package but uses unordered 'HashMap' and 'HashSet' instead of
-- ordered ones.
module Data.MultiHashMap.Set.Strict
  ( MultiHashMap

    -- * Construction
  , empty
  , singleton

    -- * Basic interface
  , null
  , size
  , member
  , memberWithValue
  , (∈), (∋), (∉), (∌)
  , lookup
  , (!)
  , insert
  , delete
  , deleteWithValue
  , alter
  , alterF
  , isSubmapOf

    -- * Combine
    -- ** Union
  , union
  , (∪)
  , unions

    -- ** Compose
  , compose

    -- * Transformations
  , map
  , mapWithKey
  , traverseWithKey
  , twist

    -- * Difference and intersection
  , difference
  , (∖)
  , intersection
  , (∩)

    -- * Folds
  , foldMapWithKey
  , foldr
  , foldl
  , foldr'
  , foldl'
  , foldrWithKey
  , foldlWithKey
  , foldrWithKey'
  , foldlWithKey'

    -- * Filter
  , filter
  , filterKey
  , filterWithKey

    -- * Conversions
  , keys
  , keysSet
  , elems
  , elemsSet

    -- ** Lists
  , toList
  , fromList
  ) where

import Control.Applicative (Applicative(..))
import Control.DeepSeq (NFData)
import qualified Data.Foldable as F
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.Bifoldable (Bifoldable(..))
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Data.MonoTraversable
  ( Element, MonoFunctor(..), MonoFoldable(..), MonoTraversable(..)
  , GrowingAppend )
import Data.Monoid.Unicode ((⊕))
import qualified GHC.Exts as Exts
import Prelude hiding (null, lookup, map, foldr, foldl, filter)
import Prelude.Unicode ((∘))


-- | @'MultiHashMap' k@ isn't a 'Functor' because its 'fmap' would
-- require @b@ to be 'Eq' and 'Hashable'. It's not a 'Traversable' for
-- the same reason. The problem stems from the lack of
-- <https://richarde.dev/papers/2020/partialdata/partialdata.pdf partial type constructors>.
newtype MultiHashMap k v
  = MultiHashMap (HashMap k (HashSet v))
  deriving (Eq, Ord, Show, NFData, Hashable)

instance Bifoldable MultiHashMap where
  bifoldMap f g = foldMapWithKey (\k v → f k ⊕ g v)
  bifoldr   f g = foldrWithKey (\k v a → k `f` (v `g` a))
  bifoldl   f g = foldlWithKey (\a k v → (a `f` k) `g` v)

instance F.Foldable (MultiHashMap k) where
  foldMap f = foldMapWithKey (\_ v → f v)
  foldr     = foldr
  foldl     = foldl
  foldr'    = foldr'
  foldl'    = foldl'
  null      = null
  length    = size

instance (Eq k, Hashable k, Eq v, Hashable v) ⇒ Exts.IsList (MultiHashMap k v) where
  type Item (MultiHashMap k v) = (k, v)
  fromList = fromList
  toList   = toList

instance (Eq k, Hashable k, Eq v, Hashable v) ⇒ Semigroup (MultiHashMap k v) where
  (<>) = (∪)

instance (Eq k, Hashable k, Eq v, Hashable v) ⇒ Monoid (MultiHashMap k v) where
  mempty = empty

type instance Element (MultiHashMap k v) = v

instance (Eq v, Hashable v) ⇒ MonoFunctor (MultiHashMap k v) where
  omap = map

instance MonoFoldable (MultiHashMap k v)
instance GrowingAppend (MultiHashMap k v)

instance (Eq v, Hashable v) ⇒ MonoTraversable (MultiHashMap k v) where
  otraverse f = traverseWithKey (const f)

unwrap ∷ MultiHashMap k v → HashMap k (HashSet v)
{-# INLINE unwrap #-}
unwrap (MultiHashMap m) = m

wrap ∷ HashMap k (HashSet v) → MultiHashMap k v
{-# INLINE wrap #-}
wrap = MultiHashMap

-- | /O(1)/ Consturct an empty map.
empty ∷ MultiHashMap k v
empty = wrap $ HM.empty

-- | /O(1)/ Construct a map with a single element.
singleton ∷ (Hashable k, Hashable v) ⇒ k → v → MultiHashMap k v
singleton k
  = wrap ∘ HM.singleton k ∘ HS.singleton

-- | /O(1)/ Return 'True' if this map is empty, 'False' otherwise.
null ∷ MultiHashMap k v → Bool
null = HM.null ∘ unwrap

-- | /O(n)/ Return the number of key-value mappings in this
-- map. Distinct values with the same key count as distinct mappings.
size ∷ MultiHashMap k v → Int
size m = HM.foldl' f 0 (unwrap m)
  where
    f ∷ Int → HashSet v → Int
    f n s = n + HS.size s

-- | /O(log k)/ Return 'True' if there is at least one value
-- associated with the given key, 'False' otherwise.
member ∷ (Eq k, Hashable k) ⇒ k → MultiHashMap k v → Bool
member = (∘ unwrap) ∘ HM.member

-- | /O(log k)/ An alias to 'member'.
(∈) ∷ (Eq k, Hashable k) ⇒ k → MultiHashMap k v → Bool
(∈) = member
infix 4 ∈

-- | /O(log k)/ @a '∋' b = 'flip' ('∈')@
(∋) ∷ (Eq k, Hashable k) ⇒ MultiHashMap k v → k → Bool
(∋) = flip (∈)
infix 4 ∋

-- | /O(log k)/ @a '∉' b = 'not' (a '∈' b)@
(∉) ∷ (Eq k, Hashable k) ⇒ k → MultiHashMap k v → Bool
(∉) = (not ∘) ∘ member
infix 4 ∉

-- | /O(log k)/ @a '∌' b = 'flip' ('∉')@
(∌) ∷ (Eq k, Hashable k) ⇒ MultiHashMap k v → k → Bool
(∌) = flip (∉)
infix 4 ∌

-- | /O(log k + log m)/ Return 'True' if the map has a pair of the
-- given key and the value, 'False' otherwise.
memberWithValue ∷ (Eq k, Hashable k, Eq v, Hashable v) ⇒ k → v → MultiHashMap k v → Bool
memberWithValue k v = HS.member v ∘ lookup k

-- | /O(log k)/. Lookup the values at a key in the map. It returns an
-- empty set if the key is not in the map.
lookup ∷ (Eq k, Hashable k) ⇒ k → MultiHashMap k v → HashSet v
lookup = (fromMaybe HS.empty ∘) ∘ (∘ unwrap) ∘ HM.lookup -- lol, what's this

-- | /O(log k)/ This is a flipped version of 'lookup'.
(!) ∷ (Eq k, Hashable k) ⇒ MultiHashMap k v → k → HashSet v
(!) = flip lookup
infixl 9 !

-- | /O(log m * log k)/. If the key exists in the multi-map, the new
-- value will be inserted into the set of values for the key. It is a
-- no-op if the value already exists in the set.
insert ∷ (Eq k, Eq v, Hashable k, Hashable v) ⇒ k → v → MultiHashMap k v → MultiHashMap k v
insert k v = wrap ∘ HM.alter f k ∘ unwrap
  where
    f (Just vs) = Just (HS.insert v vs)
    f Nothing   = Just (HS.singleton v)

-- | /O(log k)/. Delete a key and all its values from the map.
delete ∷ (Eq k, Hashable k) ⇒ k → MultiHashMap k v → MultiHashMap k v
delete = (wrap ∘) ∘ (∘ unwrap) ∘ HM.delete

-- | /O(log m * log k)/. Remove the value associated with the key if
-- exists.
deleteWithValue ∷ (Eq k, Hashable k, Eq v, Hashable v)
                ⇒ k
                → v
                → MultiHashMap k v
                → MultiHashMap k v
deleteWithValue = flip (alter ∘ HS.delete)

-- | /O(log n)/ The expression @'alter' f k m@ alters the set of
-- values at @k@, or absence thereof.
alter ∷ (Eq k, Hashable k) ⇒ (HashSet v → HashSet v) → k → MultiHashMap k v → MultiHashMap k v
alter f k m
  | HS.null vs = wrap $ HM.delete k    $ unwrap m
  | otherwise  = wrap $ HM.insert k vs $ unwrap m
  where
    vs = f (m ! k)

-- | /O(log n)/ A variant of 'alter' which takes a function returning
-- a result wrapped in some 'Functor'.
alterF ∷ (Eq k, Hashable k, Functor f)
       ⇒ (HashSet v → f (HashSet v))
       → k
       → MultiHashMap k v
       → f (MultiHashMap k v)
alterF f k m = g <$> f (m ! k)
  where
    g vs
      | HS.null vs = wrap $ HM.delete k    $ unwrap m
      | otherwise  = wrap $ HM.insert k vs $ unwrap m

-- | /O(n*log m)/ Inclusion of maps. A map is included in another map
-- if the keys are subsets and the corresponding values are also
-- subsets.
isSubmapOf ∷ (Eq k, Hashable k, Eq v, Hashable v)
           ⇒ MultiHashMap k v
           → MultiHashMap k v
           → Bool
isSubmapOf ma mb
  = HM.isSubmapOfBy HS.isSubsetOf (unwrap ma) (unwrap mb)

-- | /O(n+m)/ The union of two maps. If a key occurs in both maps, the
-- sets of values will also be merged.
union ∷ (Eq k, Hashable k, Eq v, Hashable v)
      ⇒ MultiHashMap k v
      → MultiHashMap k v
      → MultiHashMap k v
union ma mb
  = wrap $ HM.unionWith HS.union (unwrap ma) (unwrap mb)

-- | /O(n+m)/ An alias to 'union'.
(∪) ∷ (Eq k, Hashable k, Eq v, Hashable v)
    ⇒ MultiHashMap k v
    → MultiHashMap k v
    → MultiHashMap k v
(∪) = union
infixl 6 ∪

-- | Union a number of multimaps. If a key occurs in more than one
-- maps, the sets of values will also be merged.
unions ∷ (Eq k, Hashable k, Eq v, Hashable v, Foldable f)
       ⇒ f (MultiHashMap k v)
       → MultiHashMap k v
unions = F.foldl' (∪) empty

-- | Relate the keys of one map to the values of the other, by using
-- the values of the former as keys for lookups in the latter.
compose ∷ (Eq b, Hashable b, Eq c, Hashable c)
        ⇒ MultiHashMap b c
        → MultiHashMap a b
        → MultiHashMap a c
compose bc
  | null bc   = const empty
  | otherwise = wrap ∘ HM.map f ∘ unwrap
  where
    f vs    = HS.foldl' g HS.empty vs
    g acc v = HS.union acc (bc ! v)

-- | /O(n)/ Transform this map by applying a function to every value.
map ∷ (Eq v2, Hashable v2) ⇒ (v1 → v2) → MultiHashMap k v1 → MultiHashMap k v2
map = mapWithKey ∘ const

-- | /O(n)/ A variant of 'map' which applies a function to every key
-- and value.
mapWithKey ∷ (Eq v2, Hashable v2) ⇒ (k → v1 → v2) → MultiHashMap k v1 → MultiHashMap k v2
mapWithKey f = wrap ∘ HM.mapWithKey (HS.map ∘ f) ∘ unwrap

-- | /O(n)/ Perform an 'Applicative' action for each key-value pair in
-- a map and produce a map of all the results. Each map will be strict
-- in all its values.
traverseWithKey ∷ (Eq v2, Hashable v2, Applicative f)
                ⇒ (k → v1 → f v2)
                → MultiHashMap k v1
                → f (MultiHashMap k v2)
traverseWithKey f = (wrap <$>) ∘ HM.traverseWithKey g ∘ unwrap
  where
    -- HashSet is not a Traversable, probably because it would require
    -- additional constraints on the value type. We need it
    -- regardless, so we must somehow do it ourselves.
    g k      = HS.foldl' (flip $ insF k) (pure HS.empty)
    insF k v = liftA2 HS.insert (f k v)

-- | /O(n)/ Reverse the positions of keys and values in a map.
twist ∷ (Eq k, Hashable k, Eq v, Hashable v)
      ⇒ MultiHashMap k v
      → MultiHashMap v k
twist = foldrWithKey' f empty
  where
    f k v = insert v k

-- | Difference of two multi-maps.
--
-- If a key exists in the first map but not the second, it remains
-- unchanged in the result. If a key exists in both maps, a set
-- difference is performed on their values.
difference ∷ (Eq k, Hashable k, Eq v, Hashable v)
           ⇒ MultiHashMap k v
           → MultiHashMap k v
           → MultiHashMap k v
difference ma mb = wrap $ HM.differenceWith f (unwrap ma) (unwrap mb)
  where
    f as bs = case HS.difference as bs of
                vs | HS.null vs → Nothing
                   | otherwise  → Just vs

-- | An alias to 'difference'.
--
-- U+2216 SET MINUS
(∖) ∷ (Eq k, Hashable k, Eq v, Hashable v)
    ⇒ MultiHashMap k v
    → MultiHashMap k v
    → MultiHashMap k v
(∖) = difference
infixl 9 ∖

-- | Intersection of two multi-maps.
--
-- If a key exists in both maps, a set intersection is performed on
-- their values.
intersection ∷ (Eq k, Hashable k, Eq v, Hashable v)
             ⇒ MultiHashMap k v
             → MultiHashMap k v
             → MultiHashMap k v
intersection ma mb = wrap $ HM.foldlWithKey' f HM.empty (unwrap ma)
  where
    f m k as
      = case HM.lookup k (unwrap mb) of
          Just bs → case HS.intersection as bs of
                      vs | HS.null vs → m
                         | otherwise  → HM.insert k vs m
          Nothing → m

-- | An alias to 'intersection'.
(∩) ∷ (Eq k, Hashable k, Eq v, Hashable v)
      ⇒ MultiHashMap k v
      → MultiHashMap k v
      → MultiHashMap k v
(∩) = intersection
infixl 6 ∩

-- | /O(n)/ Reduce the map by applying a function to each element and
-- combining the results with a monoid operation.
foldMapWithKey ∷ Monoid m ⇒ (k → v → m) → MultiHashMap k v → m
foldMapWithKey f = HM.foldMapWithKey g ∘ unwrap
  where
    g = F.foldMap ∘ f

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldr ∷ (v → a → a) → a → MultiHashMap k v → a
foldr = foldrWithKey ∘ const

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).
foldl ∷ (a → v → a) → a → MultiHashMap k v → a
foldl = foldlWithKey ∘ (const ∘)

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator). Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' ∷ (v → a → a) → a → MultiHashMap k v → a
foldr' = foldrWithKey' ∘ const

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator). Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' ∷ (a → v → a) → a → MultiHashMap k v → a
foldl' = foldlWithKey' ∘ (const ∘)

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey ∷ (k → v → a → a) → a → MultiHashMap k v → a
foldrWithKey f a = HM.foldrWithKey g a ∘ unwrap
  where
    g = flip ∘ HS.foldr ∘ f

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).
foldlWithKey ∷ (a → k → v → a) → a → MultiHashMap k v → a
foldlWithKey f a = HM.foldlWithKey g a ∘ unwrap
  where
    g a' k = HS.foldr (\v a'' → f a'' k v) a'

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator). Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' ∷ (k → v → a → a) → a → MultiHashMap k v → a
foldrWithKey' f a = HM.foldrWithKey' g a ∘ unwrap
  where
    g k vs a' = HS.foldl' (\a'' v → f k v a'') a' vs

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator). Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' ∷ (a → k → v → a) → a → MultiHashMap k v → a
foldlWithKey' f a = HM.foldlWithKey' g a ∘ unwrap
  where
    g a' k = HS.foldl' (\a'' v → f a'' k v) a'

-- | /O(n)/ Retain all values that satisfy the predicate. A key is
-- removed if none of its values satisfies the predicate.
filter ∷ (v → Bool) → MultiHashMap k v → MultiHashMap k v
filter = filterWithKey ∘ const

-- | /O(k)/ Retain all keys that satisfy the predicate.
filterKey ∷ (k → Bool) → MultiHashMap k v → MultiHashMap k v
filterKey p = wrap ∘ HM.filterWithKey (const ∘ p) ∘ unwrap

-- | /O(n)/ Retain all key\/value pairs that satisfy the predicate. A
-- key is removed if none of its values satisfies the predicate.
filterWithKey ∷ (k → v → Bool) → MultiHashMap k v → MultiHashMap k v
filterWithKey p = wrap ∘ HM.mapMaybeWithKey f ∘ unwrap
  where
    f k vs = case HS.filter (p k) vs of
               vs' | HS.null vs' → Nothing
                   | otherwise   → Just vs'

-- | /O(n)/ Return a list of this map's keys. The list is produced
-- lazily.
keys ∷ MultiHashMap k v → [k]
keys = HM.keys ∘ unwrap

-- | /O(n)/ Produce a 'HashSet' of all the keys in the given map.
keysSet ∷ MultiHashMap k v → HashSet k
keysSet = HM.keysSet ∘ unwrap

-- | /O(n)/ Return a list of this map's values. The list is produced
-- lazily.
elems ∷ MultiHashMap k v → [v]
elems = (HS.toList =<<) ∘ HM.elems ∘ unwrap

-- | /O(n)/ Produce a 'HashSet' of all the values in the given map.
elemsSet ∷ (Eq v, Hashable v) ⇒ MultiHashMap k v → HashSet v
elemsSet = HM.foldl' HS.union HS.empty ∘ unwrap

-- | /O(n)/ Return a list of this map's elements. The list is produced
-- lazily. The order of its elements is unspecified.
toList ∷ MultiHashMap k v → [(k, v)]
toList = ((uncurry $ \k → fmap (k, ) ∘ HS.toList) =<<) ∘ HM.toList ∘ unwrap

-- | /O(n*log n)/ Build a multimap from a list of key\/value pairs.
fromList ∷ (Eq k, Hashable k, Eq v, Hashable v) ⇒ [(k, v)] → MultiHashMap k v
fromList = F.foldr (uncurry insert) empty
