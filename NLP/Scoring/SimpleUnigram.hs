{-# LANGUAGE RecordWildCards #-}

-- | This module defines a simple scoring scheme based on pairs of unigrams.

module NLP.Scoring.SimpleUnigram where

import           Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as H
import           System.IO.Unsafe (unsafePerformIO)

import           NLP.Alphabet.MultiChar



-- | Score 'MultiChar's @x@ and @y@ based on the simple scoring system: (i)
-- lookup (x,y) and use the score if found; (ii) if (x,y) is not in the
-- database, then return the default matching 'defMatch' score if @x==y@,
-- otherwise return the default mismatch 'defMismatch' score.

scoreUnigram :: SimpleScoring -> InternedMultiChar -> InternedMultiChar -> Double
scoreUnigram SimpleScoring {..} x y =
  maybe (if x==y then defMatch else defMismatch)
  id
  (unsafePerformIO $ H.lookup simpleScore (x,y))
{-# INLINE scoreUnigram #-}

-- | Collect the hashtable and scalar values for simple scoring.

data SimpleScoring = SimpleScoring
  { simpleScore  :: !(BasicHashTable (InternedMultiChar,InternedMultiChar) Double)
  , gapScore     :: !Double
  , gapOpen      :: !Double
  , gapExtend    :: !Double
  , defMatch     :: !Double
  , defMismatch  :: !Double
  }
  deriving (Show)

