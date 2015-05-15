
-- | This module keeps a persistent @bimap@ between @InternedMultiChar@s
-- and @Int@s
--
-- TODO make this a bimap @Text <-> Vector@. Compare performance when
-- printing backtracking results. (Do this after the Builder-based
-- backtracking is online)

module NLP.Alphabet.IMMC.Internal where

import Data.IORef (newIORef,IORef,readIORef,atomicWriteIORef,atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO,unsafeDupablePerformIO)

import Data.Bijection.Hash (Bimap,empty,lookupL,lookupR,size,insert)

import NLP.Alphabet.MultiChar



immcBimap :: IORef (Bimap InternedMultiChar Int)
immcBimap = unsafePerformIO $ newIORef empty
{-# NoInline immcBimap #-}

-- | Add @InternedMultiChar@ and return @Int@ key. Will return key for
-- existing string and thereby serves for lookup in left-to-right
-- direction.

immcBimapAdd :: InternedMultiChar -> Int
immcBimapAdd k = unsafeDupablePerformIO $ atomicModifyIORef' immcBimap $ \m ->
  case lookupL m k of Just i  -> (m,i)
                      Nothing -> let s = size m
                                 in  (insert m (k,s) , s)
{-# Inline immcBimapAdd #-}

-- | Lookup the @InternedMultiChar@ based on an @Int@ key. Unsafe totality
-- assumption.

immcBimapLookupInt :: Int -> InternedMultiChar
immcBimapLookupInt r = seq r . unsafeDupablePerformIO $ atomicModifyIORef' immcBimap $ \m ->
  case lookupR m r of Just l  -> (m,l)
                      Nothing -> error "immcBimapLookupInt: totality assumption invalidated"
{-# Inline immcBimapLookupInt #-}

