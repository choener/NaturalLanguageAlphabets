
module NLP.Scoring.Unigram.Default where

import Data.FileEmbed (embedFile)
import Data.Text.Encoding (decodeUtf8)

import NLP.Scoring.Unigram
import NLP.Scoring.Unigram.Import



-- | Default simple unigram scores for a system of consonants, liquid
-- consonants, and vowels of arbitrary scale.

clvDefaults = maybe (error "clvDefaults failed") id
            $ fromByteString $(embedFile "scoring/unigramdefault.score") "scoring/unigramdefault.score"

