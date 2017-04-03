
-- | This module defines a simple scoring scheme based on pairs of unigrams.

module NLP.Scoring.Unigram where

import GHC.Generics
import Data.HashMap.Strict
import Data.Aeson

import NLP.Text.BTI



-- | Score 'BTI's @x@ and @y@ based on the simple scoring system: (i)
-- lookup (x,y) and use the score if found; (ii) if (x,y) is not in the
-- database, then return the default matching 'defaultMatch' score if
-- @x==y@, otherwise return the default mismatch 'defaultMismatch' score.

scoreUnigram :: UnigramScoring -> BTI -> BTI -> Double
scoreUnigram UnigramScoring {..} x y =
  lookupDefault (if x==y then defaultMatch else defaultMismatch) (x,y) unigramMatch
{-# Inline scoreUnigram #-}

-- | Collect the hashtable and scalar values for simple scoring.
--
-- TODO binary and cereal instances

data UnigramScoring = UnigramScoring
  { unigramMatch          :: !(HashMap (BTI,BTI) Double)
  -- ^ All known matching characters and associated scores.
  , specialMismatch       :: !(HashMap BTI Double)
  -- ^ Characters that can be deleted with costs different from
  -- @gapOpen@/@gapExtension@.
  , gapLinear             :: !Double
  -- ^ linear gap scores
  , gapOpen               :: !Double
  -- ^ Gap opening costs for Gotoh-style grammars.
  , gapExtension          :: !Double
  -- ^ Gap extension costs for Gotoh-style grammars.
  , defaultMatch          :: !Double
  -- ^ Default score for characters matching, i.e. @x==y@.
  , defaultMismatch       :: !Double
  -- ^ Default score for characters not matching, i.e. @x/=y@.
  , prefixSuffixLinear    :: !Double
  -- ^ Special gap score for a prefix or suffix.
  , prefixSuffixOpen      :: !Double
  -- ^ Special gap opening score for a prefix or suffix.
  , prefixSuffixExtension :: !Double
  -- ^ Special gap extension score for a prefix or suffix.
  }
  deriving (Read,Show,Eq,Generic)

instance FromJSON UnigramScoring where
  parseJSON (Object v)
    =   UnigramScoring
    <$> (fromList `fmap` (v .: "unigramMatch"))
    <*> (fromList `fmap` (v .: "specialMismatch"))
    <*> v .: "gapLinear"
    <*> v .: "gapOpen"
    <*> v .: "gapExtension"
    <*> v .: "defaultMatch"
    <*> v .: "defaultMismatch"
    <*> v .: "prefixSuffixLinear"
    <*> v .: "prefixSuffixOpen"
    <*> v .: "prefixSuffixExtension"

instance ToJSON UnigramScoring where
  toJSON UnigramScoring {..}
    = object [ "unigramMatch"           .= toList unigramMatch
             , "specialMismatch"        .= toList specialMismatch
             , "gapLinear"              .= gapLinear
             , "gapOpen"                .= gapOpen
             , "gapExtension"           .= gapExtension
             , "defaultMatch"           .= defaultMatch
             , "defaultMismatch"        .= defaultMismatch
             , "prefixSuffixLinear"     .= prefixSuffixLinear
             , "prefixSuffixOpen"       .= prefixSuffixOpen
             , "prefixSuffixExtension"  .= prefixSuffixExtension
             ]

