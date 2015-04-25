
-- | This module keeps a persistent @bimap@ between @InternedMultiChar@s
-- and @Int@s

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
immcBimapAdd k = unsafeDupablePerformIO $ do
  m <- readIORef immcBimap
  case lookupL m k of
    Just i  -> return i
    Nothing -> do let s = size m
                  atomicModifyIORef' immcBimap $ \m -> (insert m (k,s) , s)
{-# Inline immcBimapAdd #-}

-- | Lookup the @InternedMultiChar@ based on an @Int@ key. Unsafe totality
-- assumption.

immcBimapLookupInt :: Int -> InternedMultiChar
immcBimapLookupInt r = unsafeDupablePerformIO $ do
  m <- readIORef immcBimap
  case lookupR m r of
    Just l  -> return l
    Nothing -> error "immcBimapLookupInt: totality assumption invalidated"
{-# Inline immcBimapLookupInt #-}

