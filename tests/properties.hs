
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

import           NLP.Scoring.SimpleUnigram
import           NLP.Scoring.SimpleUnigram.Default



-- | Test aeson conversion. We add random @(key,value)@ pairs in the form
-- of @xs@ to the default scoring. We also randomize the individual scoring
-- constants.
--
-- Testing is done by serialization followed by deserialization and testing
-- for equality.

prop_Aeson ( xs :: [((String,String),Double)]
           , ( (gs :: Double, go :: Double, ge :: Double)
             , (dm :: Double, di :: Double)
             , (pso :: Double, pse :: Double)
             )
           )
  = Just def' == A.decode (A.encode def')
  where def  = clvDefaults
        xs'  = fromList $ map (\((x,y),s) -> ((btiFromCS x,btiFromCS y),s)) xs
        def' = def { simpleScore  = simpleScore def `union` xs'
                   , gapScore     = gs
                   , gapOpen      = go
                   , gapExt       = ge
                   , defMatch     = dm
                   , defMismatch  = di
                   , preSufOpen   = pso
                   , preSufExt    = pse
                   }



main :: IO ()
main = $(defaultMainGenerator)

