
module NLP.Scoring.Unigram.Default where

import Data.FileEmbed (embedFile)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad.Except

import NLP.Scoring.Unigram
import NLP.Scoring.Unigram.Import



-- | Default simple unigram scores for a system of consonants, liquid
-- consonants, and vowels of arbitrary scale.

clvDefaults = either (error . errorToString) id . runExcept
            $ fromByteString $(embedFile "scoring/unigramdefault.score") "scoring/unigramdefault.score"

