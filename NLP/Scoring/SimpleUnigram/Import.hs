{-# LANGUAGE OverloadedStrings #-}

module NLP.Scoring.SimpleUnigram.Import where

import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import           Data.HashTable.IO (BasicHashTable)
import           Data.Stringable
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AB hiding (takeWhile1,skipWhile)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashTable.IO as H
import           System.IO.Unsafe (unsafePerformIO)

import           NLP.Alphabet.MultiChar
import           NLP.Scoring.SimpleUnigram



-- | Each parsed line gives a set of characters, or tells us a score.

data ParsedLine
  = PLset ByteString [InternedMultiChar]
  | PLeq ByteString Double
  | PLinset ByteString ByteString Double
  | PLgap Double
  | PLgapopen Double
  | PLgapextend Double
  | PLdefmatch Double
  | PLdefmismatch Double
  deriving (Show,Eq,Ord)

-- | Here we simple parse individual lines.

parseLine l = case AB.parseOnly (go <* AB.endOfInput) l of
                Left  err -> error $ err ++ " " ++ show l
                Right p   -> p
  where go =   PLset         <$ "Set"       <*> wd <*> mc `AB.sepBy1` AB.skipSpace -- AB.skipWhile AB.isHorizontalSpace
           <|> PLeq          <$ "Eq"        <*> wd <*> nm
           <|> PLinset       <$ "InSet"     <*> wd <*> wd <*> nm
           <|> PLgap         <$ "Gap"       <*> nm
           <|> PLgapopen     <$ "GapOpen"   <*> nm
           <|> PLgapextend   <$ "GapExtend" <*> nm
           <|> PLdefmatch    <$ "Match"     <*> nm
           <|> PLdefmismatch <$ "Mismatch"  <*> nm
        wd = AB.skipSpace *> AB.takeWhile1 (not . AB.isHorizontalSpace)
        mc = fromByteString <$> wd
        nm = AB.skipSpace *> AB.double

-- | Parses a bytestring to create a simple scoring. We don't do much error
-- checking, many of the bindings below will easily fail.
--
-- TODO obviously: implement error-checking

genSimpleScoring :: ByteString -> SimpleScoring
genSimpleScoring l = SimpleScoring t g go ge dm di
  where
    t    = unsafePerformIO $ H.fromListWithSizeHint (Prelude.length ys) ys
    ls   = B.lines l
    xs   = map parseLine ls
    ys   = concatMap genPairs $ iss ++ eqs
    sets = [s  | s@(PLset _ _)     <- xs]
    eqs  = [e  | e@(PLeq _ _)      <- xs]
    iss  = [i  | i@(PLinset _ _ _) <- xs]
    [dm] = [dm | PLdefmatch dm     <- xs]
    [di] = [di | PLdefmismatch di  <- xs]
    [g]  = [g  | PLgap g           <- xs]
    [go] = [go | PLgapopen go      <- xs]
    [ge] = [ge | PLgapextend ge    <- xs]
    genPairs (PLeq    x   d) = let ss = lookupSet x in [ ((s,s),d) | s <- ss ]
    genPairs (PLinset x y d) = let ss = lookupSet x
                                   tt = lookupSet y in [ ((s,t),d) | s <- ss, t <- tt ]
    lookupSet k = let go [] = error $ "missing set for key: " ++ show k
                      go (PLset n xs:ss) = if k==n then xs else go ss
                  in  go sets

-- | parse a simple scoring file.

simpleScoreFromFile f = B.readFile f >>= return . genSimpleScoring

