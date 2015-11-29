
-- | This module defines a simple scoring scheme based on pairs of unigrams.

module NLP.Scoring.SimpleUnigram where

import GHC.Generics
import Data.HashMap.Strict
import Data.Aeson

import NLP.Text.BTI



-- | Score 'BTI's @x@ and @y@ based on the simple scoring system: (i)
-- lookup (x,y) and use the score if found; (ii) if (x,y) is not in the
-- database, then return the default matching 'defMatch' score if @x==y@,
-- otherwise return the default mismatch 'defMismatch' score.

scoreUnigram :: SimpleScoring -> BTI -> BTI -> Double
scoreUnigram SimpleScoring {..} x y =
  lookupDefault (if x==y then defMatch else defMismatch) (x,y) simpleScore
{-# INLINE scoreUnigram #-}

-- | Collect the hashtable and scalar values for simple scoring.
--
-- TODO binary and cereal instances

data SimpleScoring = SimpleScoring
  { simpleScore  :: !(HashMap (BTI,BTI) Double)
  , gapScore     :: !Double
  , gapOpen      :: !Double
  , gapExtend    :: !Double
  , defMatch     :: !Double
  , defMismatch  :: !Double
  }
  deriving (Read,Show,Eq,Generic)

instance FromJSON SimpleScoring where
  parseJSON (Object v) = SimpleScoring <$>
                          (fromList `fmap` (v .: "simpleScore")) <*>
                          v .: "gapScore"    <*>
                          v .: "gapOpen"     <*>
                          v .: "gapExtend"   <*>
                          v .: "defMatch"    <*>
                          v .: "defMismatch"

instance ToJSON SimpleScoring where
  toJSON (SimpleScoring ss gs go ge dm di)
    = object [ "simpleScore" .= toList ss
             , "gapScore"    .= gs
             , "gapOpen"     .= go
             , "gapExtend"   .= ge
             , "defMatch"    .= dm
             , "defMismatch" .= di
             ]

