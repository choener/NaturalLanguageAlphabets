
module NLP.Scoring.SimpleUnigram.Default where

import Data.FileEmbed (embedFile)
import Data.Text.Encoding (decodeUtf8)

import NLP.Scoring.SimpleUnigram
import NLP.Scoring.SimpleUnigram.Import



-- | Default simple unigram scores for a system of consonants, liquid
-- consonants, and vowels of arbitrary scale.

clvDefaults = genSimpleScoring $ decodeUtf8 $(embedFile "scoring/simpleunigram.score")

