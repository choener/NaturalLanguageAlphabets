
module Main where

import           Control.Applicative
import           Data.String
import           Data.Stringable hiding (fromString)
import           Debug.Trace
import qualified Data.Aeson as A
import qualified Data.Binary as B
import qualified Data.Serialize as S
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH

import           NLP.Alphabet.IMMC



-- * IMMC

-- basic property of interning

prop_IMMC (t :: String)
  | t == u    = True
  | otherwise = traceShow (t, getIMMC i, u) False
  where i :: IMMC = fromString t
        u         = toString   i

-- binary

prop_Binary (t :: String) = t == toString j
  where i :: IMMC = fromString t
        j :: IMMC = B.decode $ B.encode i

-- cereal

prop_Serialize (t :: String) = Right t == (toString <$> j)
  where i ::               IMMC = fromString t
        j :: Either String IMMC = S.decode $ S.encode i

-- aeson (more complicated to due the json format!

prop_Aeson (t :: String) = Just [t] == (map toString <$> j)
  where i ::       [IMMC] = [fromString t]
        j :: Maybe [IMMC] = A.decode $ A.encode i

main :: IO ()
main = $(defaultMainGenerator)

