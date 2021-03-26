{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Concurrent IO-based LRU cache. This is similar to
-- <http://hackage.haskell.org/package/arbor-lru-cache arbor-lru-cache>
-- but instead of STM this uses 'MVar' and 'QSem' for
-- synchronisation so it's *slightly* safer to use in
-- `System.IO.Unsafe.unsafePerformIO`.
module Control.Concurrent.Cache.LRU
  ( LRU
  , new
  , lookup
  , entries
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  ( MVar, newMVar, newEmptyMVar, readMVar
  , putMVar, modifyMVar, modifyMVar_ )
import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)
import Control.Exception
  ( SomeException, assert, bracket_, onException, catch, throwIO )
import Control.Monad (join, void)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq((:|>)), (><), (|>))
import qualified Data.Sequence as S
import Lens.Micro ((&), (.~), (%~), (^.))
import Lens.Micro.TH (makeLenses)
import Prelude hiding (lookup)
import Prelude.Unicode ((≡), (≥), (∘))


data LRUMap k v
  = LRUMap
    { -- | When a value is being retrieved, the corresponding MVar
      -- becomes empty. If it's 'Left' it means the retriever threw an
      -- exception.
      _lmEntries   ∷ !(HashMap k (MVar (Either SomeException v)))
      -- | Eviction queue: sorted in LRU order.
    , _lmEvictionQ ∷ !(Seq k)
    }

makeLenses ''LRUMap

data LRU k v
  = LRU
    { _lruCapacity    ∷ !Int
    , _lruSemInFlight ∷ !QSem
    , _lruEntries     ∷ !(MVar (LRUMap k v))
    , _lruRetrieve    ∷ !(k → IO v)
    , _lruEvict       ∷ !(k → v → IO ())
    }

makeLenses ''LRU

-- | Create an empty LRU cache.
--
-- The retrieval procedure will be invoked for keys missing in the
-- cache. If the same key is looked up multiple times, the cache
-- guarantees that the retrieval procedure is invoked only once for
-- that key up until it is evicted.
--
-- The eviction procedure will be invoked when an entry is
-- evicted. Please be aware that if your code is concurrent, the
-- eviction procedure may be called whilst the you code is
-- concurrently using a value it has looked up. Your code is wholly
-- responsible for ensuring this case still works.
--
new ∷ Int -- ^ Maximum number of retrieval in flight
    → Int -- ^ Capacity of the LRU cache
    → (k → IO v)      -- ^ Retrieval procedure
    → (k → v → IO ()) -- ^ Eviction procedure
    → IO (LRU k v)
new maxR cap ret evi
  = assert (maxR ≥ 0) $
    assert (cap  ≥ 0) $
    do sem ← newQSem maxR
       lm  ← newMVar LRUMap
               { _lmEntries   = HM.empty
               , _lmEvictionQ = S.empty
               }
       return LRU
         { _lruCapacity    = cap
         , _lruSemInFlight = sem
         , _lruEntries     = lm
         , _lruRetrieve    = ret
         , _lruEvict       = evi
         }

-- | Look up a value in the LRU cache, or invoke the retrieval
-- procedure if it's missing.
lookup ∷ (Hashable k, Eq k) ⇒ k → LRU k v → IO v
lookup k lru
  = lookup' `onException` removeCell
  where
    lookup'
      = join $ modifyMVar (lru^.lruEntries) $ \lm →
          case HM.lookup k (lm^.lmEntries) of
            Just cell →
              do let !lm' = lm & lmEvictionQ %~ touch k
                 return (lm', evalCell cell)

            Nothing →
              do -- We don't know if we can acquire the semaphore at
                -- this point, but we must create an empty cell here
                -- because otherwise there is no way to fill it
                -- afterwards.
                cell ← newEmptyMVar
                let !lm' = lm & lmEntries %~ HM.insert k cell
                return (lm', fillCell cell)

    removeCell
      = modifyMVar_ (lru^.lruEntries) (return∘(& lmEntries %~ HM.delete k))

    evalCell cell
      = do ev ← readMVar cell
           case ev of
             Right v → return v
             Left  e → throwIO e

    -- Invariant: 'cell' must be empty.
    fillCell cell
      = do let sem = lru^.lruSemInFlight
           v ← bracket_ (waitQSem sem) (signalQSem sem) $
               do v ← catch ((lru^.lruRetrieve) k) $ \(e ∷ SomeException) →
                    do putMVar cell (Left e)
                       -- Throwing an exception here will invoke
                       -- 'removeCell'.
                       throwIO e
                  -- Reaching here means that the retriever didn't
                  -- throw. We can still be killed by an asynchronous
                  -- exception, in which case 'removeCell' will be
                  -- invoked.
                  putMVar cell (Right v)
                  return v
           -- Now that we have retrieved the value, we can release the
           -- semaphore here.
           kvsToEvict ← modifyMVar (lru^.lruEntries) $ \lm →
             let !lm'         = lm & lmEvictionQ %~ touch k
                 (!es, !lm'') = takeEvictionsDue (lru^.lruCapacity) lm'
             in
               return (lm'', es)
           -- Values to be evicted might not be available yet. We
           -- still want to invoke the eviction procedure for them,
           -- but we also don't want to waste time waiting for them to
           -- arrive. So do it in a separate thread.
           void $ forkIO $ traverse_ (uncurry $ evict (lru^.lruEvict)) kvsToEvict
           return v

-- | Get all the keys and values contained in the LRU cache. This
-- function blocks if some of the values aren't available yet.
entries ∷ LRU k v → IO (HashMap k v)
entries lru
  = do lm ← readMVar (lru^.lruEntries)
       flip traverse (lm^.lmEntries) $ \cell →
         do mv ← readMVar cell
            case mv of
              Right v → return v
              Left  e → throwIO e

touch ∷ Eq k ⇒ k → Seq k → Seq k
touch !k !ks
  = let go !as S.Empty    = as
        go !as (bs :|> b)
          | b ≡ k         = as >< bs
          | otherwise     = go (as |> b) bs
    in
      go S.empty ks |> k

takeEvictionsDue ∷ (Hashable k, Eq k)
                 ⇒ Int
                 → LRUMap k v
                 → ( Seq (k, MVar (Either SomeException v))
                   , LRUMap k v
                   )
takeEvictionsDue !cap !lm
  = let (!ks, !ks') = splitQueue cap (lm^.lmEvictionQ)
        (!es, !es') = mapAndDelete ks (lm^.lmEntries)
        !lm'        = lm & lmEvictionQ .~ ks'
                        & lmEntries   .~ es'
    in
      (es, lm')

splitQueue ∷ Int → Seq k → (Seq k, Seq k)
splitQueue !cap !ks0
  | S.length ks0 > cap = go (S.length ks0 - cap) S.empty ks0
  | otherwise          = (S.empty, ks0)
  where
    go 0 !as !bs        = (as, bs)
    go n !as (bs :|> b) = go (n-1) (as |> b) bs
    go _ _   S.Empty    = error "impossible"

mapAndDelete ∷ (Hashable k, Eq k)
             ⇒ Seq k
             → HashMap k v
             → (Seq (k, v), HashMap k v)
mapAndDelete !ks0 !m0 = go S.empty ks0 m0
  where
    go !kvs S.Empty    !m = (kvs, m)
    go !kvs (ks :|> k) !m = let !v  = m HM.! k
                                !m' = HM.delete k m
                            in
                              go (kvs |> (k, v)) ks m'

evict ∷ (k → v → IO ()) → k → MVar (Either e v) → IO ()
evict f k cell
  = do ev ← readMVar cell
       case ev of
         Right v → f k v
         Left _  → return () -- Failed to retrieve it. No need to evict.
