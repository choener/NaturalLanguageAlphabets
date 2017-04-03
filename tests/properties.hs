
module Main where

import           Control.Applicative
import           Data.HashMap.Strict (fromList,union)
import           Debug.Trace
import qualified Data.Aeson as A
import qualified Data.Binary as B
import qualified Data.Serialize as S
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           NLP.Text.BTI

import           NLP.Scoring.Unigram
import           NLP.Scoring.Unigram.Default



-- | Test aeson conversion. We add random @(key,value)@ pairs in the form
-- of @xs@ to the default scoring. We also randomize the individual scoring
-- constants.
--
-- Testing is done by serialization followed by deserialization and testing
-- for equality.

prop_Aeson ( xs :: [((String,String),Double)]
           , smm :: [(String,Double)]
           , ( (gl :: Double, go :: Double, ge :: Double)
             , (dm :: Double, di :: Double)
             , (psl :: Double, pso :: Double, pse :: Double)
             )
           )
  = Just def' == (either error id $ A.eitherDecode (A.encode def'))
  where def  = clvDefaults
        xs'  = fromList $ map (\((x,y),s) -> ((btiFromCS x,btiFromCS y),s)) xs
        smm' = fromList $ map (\(x,s) -> (btiFromCS x, s)) smm
        def' = def { unigramMatch           = unigramMatch def `union` xs'
                   , specialMismatch        = specialMismatch def `union` smm'
                   , gapLinear              = gl
                   , gapOpen                = go
                   , gapExtension           = ge
                   , defaultMatch           = dm
                   , defaultMismatch        = di
                   , prefixSuffixLinear     = psl
                   , prefixSuffixOpen       = pso
                   , prefixSuffixExtension  = pse
                   }



main :: IO ()
main = $(defaultMainGenerator)

