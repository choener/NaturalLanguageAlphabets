
-- |
--
-- TODO normalization of characters!

module NLP.Scoring.Unigram.Import where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Class
import           Control.Monad.Trans.State.Strict hiding (gets)
import           Data.HashMap.Strict (fromList, HashMap)
import           Data.HashSet (HashSet)
import           Data.String (IsString)
import           Data.Text (Text)
import           Debug.Trace
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Parser.LookAhead
import           Text.Parser.Token.Style
import           Text.Trifecta as TT
import qualified Data.Sequence as S
import           Data.Monoid
import           Data.Char

import           NLP.Text.BTI

import           NLP.Scoring.Unigram



data Env = Env
  { _equalChars     :: HashSet (Text, [Text])
  , _similarChars   :: HashSet (Text, [Text])
  , _warnings       :: S.Seq Text
  , _equalScores    :: HashMap Text Double
  , _similarScores  :: HashMap (Text,Text) Double
  , _constants      :: HashMap Text Double
  }
  deriving (Show)

makeLenses ''Env

defaultEnv = Env
  { _equalChars     = HS.empty
  , _similarChars   = HS.empty
  , _warnings       = S.empty
  , _equalScores    = HM.empty
  , _similarScores  = HM.empty
  , _constants      = HM.empty
  }



test = fromFile "scoring/simpleunigram.score"

fromFile :: FilePath -> IO (Maybe UnigramScoring)
fromFile fp = do
  p' <- TT.parseFromFile ((runStateT . runUnigramParser) pUnigram defaultEnv) fp
  case p' of
    Nothing    -> return Nothing
    Just (p,e) -> do
      let ws = e^.warnings
      unless (null ws) $ do
        mapM_ T.putStrLn ws
      return $ Just p

pUnigram :: UnigramParser UnigramScoring
pUnigram = do
  someSpace
  many $ choice [pEqSet, pSet, pEqualScores, pSimilarScores, pConstants]
  eof
  let uconstants :: Text -> UnigramParser Double
      uconstants k = do
        kv <- use constants
        case HM.lookup k kv of
          Nothing -> do
            warnings %= (S.|> ("constant " <> k <> " not found, using default (-999999)"))
            return (-999999)
          Just v  -> return v
  -- now we create the list of all matches of characters (which includes
  -- similar and dissimilar characters) and maximize over the possible
  -- scores.
  let equalchars = []
  let unigramMatch = HM.fromListWith max $ equalchars
  let specialMismatch = HM.empty
  gapLinear     <- uconstants "Gap"
  gapOpen       <- uconstants "GapOpen"
  gapExtension  <- uconstants "GapExtension"
  defaultMatch    <- uconstants "Match"
  defaultMismatch <- uconstants "Mismatch"
  prefixSuffixLinear    <- uconstants "PrefixSuffixLinear"
  prefixSuffixOpen      <- uconstants "PrefixSuffixOpen"
  prefixSuffixExtension <- uconstants "PrefixSuffixExtension"
  -- Given the @Env@, we can now construct the actual scoring system.
  return UnigramScoring{..}

-- | Read a line containing an "EqSet". Will only parse successfully if the
-- set is not yet known. Inserts the set into the @Env@.

pEqSet :: UnigramParser ()
pEqSet = do
  ks <- uses equalScores HM.keys
  reserve reserved "EqSet"
  ty <- choice $ map textSymbol ks
  vs <- runUnlined $ some pGrapheme
  someSpace
  equalChars %= (HS.insert (ty, vs))

pSet :: UnigramParser ()
pSet = do
  reserve reserved "Set"
  ty <- ident reserved
  vs <- runUnlined $ some pGrapheme
  someSpace
  similarChars %= HS.insert (ty,vs)
  return ()

pEqualScores :: UnigramParser ()
pEqualScores = do
  reserve reserved "Eq"
  ty <- ident reserved
  v  <- either fromIntegral id <$> integerOrDouble
  equalScores %= HM.insert ty v

pSimilarScores :: UnigramParser ()
pSimilarScores = do
  reserve reserved "InSet"
  ty1 <- ident reserved
  ty2 <- ident reserved
  v <- either fromIntegral id <$> integerOrDouble
  similarScores %= HM.insert (ty1,ty2) v

-- | Small parsers for the different constants we have.
--
-- TODO bail if we see a constant twice?

pConstants :: UnigramParser ()
pConstants = choice $ map pConstant cs
  where
    cs = [ "Gap", "GapOpen", "GapExtend", "PrefixSuffixOpen", "PrefixSuffixExtend"
         , "Match", "Mismatch"
         ]
    pConstant :: Text -> UnigramParser ()
    pConstant r = do
      reserveText reserved r
      v <- either fromIntegral id <$> integerOrDouble
      constants %= HM.insert r v

pGrapheme :: (CharParsing p, TokenParsing p) => p Text
pGrapheme = T.pack <$> some (satisfy (not . isSpace)) <* someSpace

-- | TODO only insert warning, not error, after seeing a character again!

setIdent :: HashSet Text -> Unlined UnigramParser Text
setIdent e = try $ do
  k <- ident reserved
  when (HS.member k e) $ fail "Character already present in EqSet!"
  return k

reserved :: TokenParsing m => IdentifierStyle m
reserved = emptyIdents { _styleReserved = rs }
  where rs = HS.fromList [ "EqSet", "Set", "Eq", "InSet"
                         ]


newtype UnigramParser a = UnigramParser { runUnigramParser :: StateT Env Parser a }
  deriving
    ( Alternative
    , Applicative
    , CharParsing
    , Functor
    , Monad
    , MonadPlus
    , MonadState Env
    , Parsing
    , LookAheadParsing
    )

instance TokenParsing UnigramParser where
  someSpace = buildSomeSpaceParser (() <$ space) haskellCommentStyle


{-

-- | Each parsed line gives a set of characters, or tells us a score.
--
-- TODO add @LPimport@ which starts a recursive import (note: start by storing
-- the hash or whatever of the file to be imported so that we can comment on
-- circular imports)

data ParsedLine
  = PLset Text [BTI]
  | PLeq Text Double
  | PLeqset Text [BTI]
  | PLinset Text Text Double
  | PLgap Double
  | PLgapopen Double
  | PLgapextend Double
  | PLdefmatch Double
  | PLdefmismatch Double
  | PLpresufOpen Double
  | PLpresufExt Double
  | PLcomment Text
  deriving (Show,Eq,Ord)

-- | Here we simple parse individual lines.

parseLine :: Text -> ParsedLine
parseLine l = case AT.parseOnly (go <* AT.endOfInput) l of
                Left  err -> error $ err ++ " " ++ show l
                Right p   -> p
  where go =   PLset         <$ "Set"           <*> wd <*> mc `AT.sepBy1` AT.skipSpace -- AB.skipWhile AB.isHorizontalSpace
           <|> PLeq          <$ "Eq"            <*> wd <*> nm
           <|> PLinset       <$ "InSet"         <*> wd <*> wd <*> nm
           <|> (PLgap         <$ "Gap"           <*> nm  <?> "(linear) Gap")
           <|> (PLgapopen     <$ "GapOpen"       <*> nm  <?> "GapOpen")
           <|> (PLgapextend   <$ "GapExtend"     <*> nm  <?> "GapExtend")
           <|> (PLdefmatch    <$ "Match"         <*> nm  <?> "Match")
           <|> (PLdefmismatch <$ "Mismatch"      <*> nm  <?> "Mismatch")
           <|> (PLpresufOpen  <$ "PreSufOpen"    <*> nm  <?> "PreSufOpen")
           <|> (PLpresufExt   <$ "PreSufExtend"  <*> nm  <?> "PreSufExtend")
           <|> PLeqset       <$ "EqSet"         <*> wd <*> mc `AT.sepBy1` AT.skipSpace
           <|> PLcomment     <$ "--"            <*> AT.takeText
        wd = AT.skipSpace *> AT.takeWhile1 (not . AT.isHorizontalSpace)
        mc = bti <$> wd
        nm = AT.skipSpace *> AT.double

-- | Parses a bytestring to create a simple scoring. We don't do much error
-- checking, many of the bindings below will easily fail.
--
-- TODO obviously: implement error-checking

genSimpleScoring :: Text -> SimpleScoring
genSimpleScoring l = SimpleScoring{..} -- SimpleScoring t g go ge dm di
  where
    simpleScore = fromList ys
    [defMatch]    = [dm | PLdefmatch dm     <- xs]
    [defMismatch] = [di | PLdefmismatch di  <- xs]
    [gapScore]    = [g  | PLgap g           <- xs]
    [gapOpen]     = [go | PLgapopen go      <- xs]
    [gapExt]      = [ge | PLgapextend ge    <- xs]
    [preSufOpen]  = [ps | PLpresufOpen ps   <- xs]
    [preSufExt]   = [ps | PLpresufExt ps    <- xs]
    ls   = T.lines l
    xs   = map parseLine ls
    ys   = concatMap genPairs $ iss ++ eqs
    sets = [s  | s@(PLset _ _)     <- xs]
    eqss = [e  | e@(PLeqset _ _)   <- xs]
    eqs  = [e  | e@(PLeq _ _)      <- xs]
    iss  = [i  | i@(PLinset _ _ _) <- xs]
    -- this generates all "equality pairs", i.e. that 'a' == 'a'
    -- the second list generates all equivalence classes, say that 'a' == 'ã'
    genPairs (PLeq    x   d) = let ss = lookupSet x
                                   tt = lookupEqSet x
                               in  [ ((s,s),d) | s <- ss ] ++
                                   [ ((s,t),d) | ts <- tt, s<-ts,t<-ts ]
    -- this creates all variants of, say, vowel against vowel (but unequal)
    genPairs (PLinset x y d) = let ss = lookupSet x
                                   tt = lookupSet y in [ ((s,t),d) | s <- ss, t <- tt ]
    -- find every character from an equivalence set
    lookupEqSet k = let go [] = []
                        go (PLeqset n xs:ss) = if k==n then xs : go ss else go ss
                    in  go eqss
    -- find every character from a certain class
    lookupSet k = let go [] = []
                      go (PLset n xs:ss) = if k==n then xs : go ss else go ss
                      go (PLeqset n xs:ss) = if k==n then xs : go ss else go ss
                  in  case go $ sets ++ eqss of
                        xs -> concat xs

-- | parse a simple scoring file.

simpleScoreFromFile f = T.readFile f >>= return . genSimpleScoring

-}

