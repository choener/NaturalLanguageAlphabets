
-- | This module defines a simple scoring scheme based on pairs of unigrams.

module NLP.Scoring.Unigram where

import Data.Aeson
import Data.HashMap.Strict
import GHC.Generics

import NLP.Text.BTI



-- | Score 'BTI's @x@ and @y@ based on the simple scoring system: (i)
-- lookup (x,y) and use the score if found; (ii) if (x,y) is not in the
-- database, then return the default matching 'defaultMatch' score if
-- @x==y@, otherwise return the default mismatch 'defaultMismatch' score.

matchUnigram :: UnigramScoring -> BTI -> BTI -> Double
matchUnigram UnigramScoring{..} x y =
  lookupDefault (if x==y then usDefaultMatch else usDefaultMismatch) (x,y) usUnigramMatch
{-# Inline matchUnigram #-}

-- | Provides a score for the unigram characters in an @in/del@
-- environment. In case the character @x@ in the pairing @x == '-'@ is
-- found in the @unigramInsert@ database, that score is used, otherwise the
-- @gapLinear@ score is used.

insertUnigram ∷ UnigramScoring → BTI → Double
insertUnigram UnigramScoring{..} x =
  lookupDefault usGapLinear x usUnigramInsert
{-# Inline insertUnigram #-}

-- TODO $UTF-Vowels , etc in parsing ?!

-- | Collect the hashtable and scalar values for simple scoring.
--
-- TODO binary and cereal instances

data UnigramScoring = UnigramScoring
  { usUnigramMatch          :: !(HashMap (BTI,BTI) Double)
  -- ^ All known matching characters and associated scores.
  , usUnigramInsert         :: !(HashMap BTI Double)
  -- ^ Characters that can be deleted with costs different from
  -- @gapOpen@/@gapExtension@.
  , usGapLinear             :: !Double
  -- ^ linear gap scores
  , usGapOpen               :: !Double
  -- ^ Gap opening costs for Gotoh-style grammars.
  , usGapExtension          :: !Double
  -- ^ Gap extension costs for Gotoh-style grammars.
  , usDefaultMatch          :: !Double
  -- ^ Default score for characters matching, i.e. @x==y@.
  , usDefaultMismatch       :: !Double
  -- ^ Default score for characters not matching, i.e. @x/=y@.
  , usPrefixSuffixLinear    :: !Double
  -- ^ Special gap score for a prefix or suffix.
  , usPrefixSuffixOpen      :: !Double
  -- ^ Special gap opening score for a prefix or suffix.
  , usPrefixSuffixExtension :: !Double
  -- ^ Special gap extension score for a prefix or suffix.
  }
  deriving (Read,Show,Eq,Generic)

instance FromJSON UnigramScoring where
  parseJSON (Object v)
    =   UnigramScoring
    <$> (fromList `fmap` (v .: "unigramMatch"))
    <*> (fromList `fmap` (v .: "unigramInsert"))
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
    = object [ "unigramMatch"           .= toList usUnigramMatch
             , "unigramInsert"          .= toList usUnigramInsert
             , "gapLinear"              .= usGapLinear
             , "gapOpen"                .= usGapOpen
             , "gapExtension"           .= usGapExtension
             , "defaultMatch"           .= usDefaultMatch
             , "defaultMismatch"        .= usDefaultMismatch
             , "prefixSuffixLinear"     .= usPrefixSuffixLinear
             , "prefixSuffixOpen"       .= usPrefixSuffixOpen
             , "prefixSuffixExtension"  .= usPrefixSuffixExtension
             ]

