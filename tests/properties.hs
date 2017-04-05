
module Main where

import           Control.Applicative
import           Data.HashMap.Strict (fromList,union)
import           Debug.Trace
import qualified Data.Aeson as A
import qualified Data.Binary as B
import qualified Data.Serialize as S
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH
import           Data.Either
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (lookupDefault, lookup)
import           Prelude hiding (lookup)

import           NLP.Text.BTI

import           NLP.Scoring.Unigram
import           NLP.Scoring.Unigram.Default
import           NLP.Scoring.Unigram.Import



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

-- Everything here should succeed

case_Import_uni01 = do
  eu <- fromFile False "./tests/uni01.score"
  assertBool "uni01 load should succeed" $ isRight eu
  let Right u = eu
  assertEqual "EqualScore Consonant 4 F~f" (Just 4) . lookup (bti "F", bti "f") $ unigramMatch u
  assertEqual "GapLinear" (-4) $ gapLinear u
  return ()

-- Here we test that the parser should fail on wrong input

case_Import_uni02 = do
  eu <- fromFile False "./tests/uni02.score"
  assertBool "uni02 load should fail" $ isLeft eu
  let Left u = eu
  return ()

main :: IO ()
main = $(defaultMainGenerator)

