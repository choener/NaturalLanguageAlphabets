
-- | Working with natural languages entails having large, sparse scoring
-- matrices. Here, we run through a bunch of options for these.
--
-- We create a table to look things up, and two sets of queries: one of
-- known elements, one of unknown elements.

module Main where

import           Control.Applicative ((<$>))
import           Control.DeepSeq
import           Control.Monad
import           Criterion.Main
import           Data.String
import           Data.Tuple (swap)
import qualified Data.HashMap.Strict as UCHS
import qualified Data.HashTable.IO as HT
import qualified Data.IntMap.Strict  as CIS
import qualified Data.Vector as VV
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import           System.IO.Unsafe (unsafePerformIO)
import           System.Random
import           System.Random.MWC
import           Text.Printf

import NLP.Text.BTI



main :: IO ()
main = do ( !keys , !unks , (!hmsH,!cisH,!htH) , (!hmsK,!cisK,!htK) ) <- setupEnv
          printf "We have %d keys, and %d / %d / %d resp. %d / %d / %d keys in the lookup maps\n"
                    (VU.length keys)
                    (UCHS.size hmsH) (CIS.size cisH) (-1 :: Int)
                    (UCHS.size hmsK) (CIS.size cisK) (-1 :: Int)
          defaultMain
            [ bgroup "  100 keys" [ bench "HashMap.Strict" $ whnf (\k -> UCHS.lookupDefault  0 k hmsH) (VU.head keys)
                                  , bench "IntMap.Strict" $ whnf (\k -> CIS.findWithDefault 0 k cisH) (getBTI $ VU.head keys)
                                  , bench "HashTable" $ whnf (\k -> htLookup htH k)               (VU.head keys)
                                  ]
            , bgroup "10000 keys" [ bench "HashMap.Strict" $ whnf (\k -> UCHS.lookupDefault  0 k hmsK) (VU.head keys)
                                  , bench "IntMap.Strict" $ whnf (\k -> CIS.findWithDefault 0 k cisK) (getBTI $ VU.head keys)
                                  , bench "HashTable" $ whnf (\k -> htLookup htK k)               (VU.head keys)
                                  ]
--            , bgroup "  100 known"   [ bench "uchsH" $ whnf (\ks -> VU.sum $ VU.map (\k -> UCHS.lookupDefault  0 k hmsH) ks) (VU.take 100 keys)
--                                     , bench " cisH" $ whnf (\ks -> VU.sum $ VU.map (\k -> CIS.findWithDefault 0 k cisH) ks) (VU.map getBTI $ VU.take 100 keys)
--                                     ]
--            , bgroup "    1 unknown" [ bench "uchsH" $ whnf (\k -> UCHS.lookupDefault  0 k hmsH) (VU.head unks)
--                                     , bench " cisH" $ whnf (\k -> CIS.findWithDefault 0 k cisH) (getBTI $ VU.head unks)
--                                     ]
            ]

htLookup ht k = unsafePerformIO $ do
  l <- HT.lookup ht k
  case l of
    Nothing -> return 0
    Just v  -> return v
{-# Inline htLookup #-}


type HTB = HT.BasicHashTable BTI Double

setupEnv = do
  -- create keys
  strs :: [String] <- replicateM 10000 rString
  -- scores to look up
  scrs :: [Double] <- replicateM 10000 $ randomRIO (0 , 9)
  -- create BTI keys
  let keys = map (bti . fromString) strs
  -- create random keys, mostly not in strs
  unks :: [String] <- replicateM 10000 rString
  let sknu = map (bti . fromString) unks
  -- for 100 keys
  let hmsK = UCHS.fromList $ take 100 $ zip keys              scrs
  let cisK =  CIS.fromList $ take 100 $ zip (map getBTI keys) scrs
  htH :: HTB <- HT.fromList $ take 100 $ zip keys scrs
  -- for 10 000 keys
  let hmsM = UCHS.fromList $ zip keys              scrs
  let cisM =  CIS.fromList $ zip (map getBTI keys) scrs
  htK :: HTB <- HT.fromList $ zip keys scrs
  return (VU.fromList keys , VU.fromList sknu , (hmsK,cisK,htH) , (hmsM,cisM,htK) )

rString :: IO String
rString = do
  k :: Int <- randomRIO (1,4)
  replicateM k $ randomRIO ('A','Z')

