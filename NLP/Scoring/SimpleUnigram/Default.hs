
module NLP.Scoring.SimpleUnigram.Default where

import Data.FileEmbed (embedFile)

import NLP.Scoring.SimpleUnigram
import NLP.Scoring.SimpleUnigram.Import



-- | Default simple unigram scores for a system of consonants, liquid
-- consonants, and vowels of arbitrary scale.

clvDefaults = genSimpleScoring $(embedFile "scoring/simpleunigram.txt")

