
-- |
--
-- TODO normalization of characters!

module NLP.Scoring.Unigram.Import where

import           Control.Applicative
import           Control.Arrow (first, (***))
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Class
import           Control.Monad.Trans.State.Strict hiding (gets)
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.HashMap.Strict (fromList, HashMap)
import           Data.HashSet (HashSet)
import           Data.Maybe
import           Data.Monoid
import           Data.String (IsString)
import           Data.Text (Text)
import           Debug.Trace
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Parser.LookAhead
import           Text.Parser.Token.Style
import           Text.Trifecta as TT
import qualified Data.ByteString.UTF8 as UTF8
import           Text.Trifecta.Delta (Delta(..))

import           NLP.Text.BTI

import           NLP.Scoring.Unigram



data Env = Env
  { _equalChars     :: HashSet (Text, [Text])
  , _similarChars   :: HashSet (Text, [Text])
  , _warnings       :: S.Seq Text
  , _equalScores    :: HashMap Text Double
  , _similarScores  :: HashMap (Text,Text) Double
  , _constants      :: HashMap Text Double
  , _ignoredChars   :: HashSet Text
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
  , _ignoredChars   = HS.empty
  }



test = fromFile "scoring/simpleunigram.score"

fromByteString :: ByteString -> String -> Maybe UnigramScoring
fromByteString s fn = r where
  p = parseByteString ((runStateT . runUnigramParser) pUnigram defaultEnv)
                      (Directed (UTF8.fromString fn) 0 0 0 0) s
  r = case p of
        Success (p',e) -> Just p'
        e              -> error $ show e

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
  many $ choice [pEqualChars, pSimilarChars, pEqualScores, pSimilarScores, pConstants, pIgnored]
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
  similarchars <- do
    scs <- use similarChars
    sss <- use similarScores
    return [ ((x,y),v)
           | (s,xs) <- HS.toList scs
           , (t,ys) <- HS.toList scs
           , let mv = HM.lookup (s,t) sss
           , isJust mv
           , let Just v = mv
           , x <- xs, y <- ys
           ]
  equalchars <- do
    ecs <- use equalChars
    ess <- use equalScores
    return [ ((x,y),v)
           | (s,xs) <- HS.toList ecs
           , let mv = HM.lookup s ess
           , isJust mv
           , let Just v = mv
           , x <- xs, y <- xs
           ]
  let unigramMatch = HM.fromListWith max . map (first (bti***bti)) $ similarchars ++ equalchars
  -- TODO fill this
  let specialMismatch = HM.empty -- TODO
  gapLinear     <- uconstants "GapLinear"
  gapOpen       <- uconstants "GapOpen"
  gapExtension  <- uconstants "GapExtension"
  defaultMatch    <- uconstants "Match"
  defaultMismatch <- uconstants "Mismatch"
  prefixSuffixLinear    <- uconstants "PrefixSuffixLinear"
  prefixSuffixOpen      <- uconstants "PrefixSuffixOpen"
  prefixSuffixExtension <- uconstants "PrefixSuffixExtension"
  -- Given the @Env@, we can now construct the actual scoring system.
  return UnigramScoring{..}

-- | Read a line containing an "EqualChars". Will only parse successfully if the
-- set is not yet known. Inserts the set into the @Env@.

pEqualChars :: UnigramParser ()
pEqualChars = do
  ks <- uses equalScores HM.keys
  reserve reserved "EqualChars"
  when (null ks) $ fail "no EqualChars's defined!"
  ty <- try $ choice $ map textSymbol ks
  vs <- runUnlined $ some pGrapheme
  someSpace
  equalChars %= (HS.insert (ty, vs))

pSimilarChars :: UnigramParser ()
pSimilarChars = do
  reserve reserved "SimilarChars"
  ty <- ident reserved
  vs <- runUnlined $ some pGrapheme
  someSpace
  similarChars %= HS.insert (ty,vs)

pEqualScores :: UnigramParser ()
pEqualScores = do
  reserve reserved "EqualScore"
  ty <- ident reserved
  v  <- either fromIntegral id <$> integerOrDouble
  equalScores %= HM.insert ty v

pSimilarScores :: UnigramParser ()
pSimilarScores = do
  reserve reserved "SimilarScore"
  ty1 <- ident reserved
  ty2 <- ident reserved
  v <- either fromIntegral id <$> integerOrDouble
  similarScores %= HM.insert (ty1,ty2) v

pIgnored :: UnigramParser ()
pIgnored = do
  reserve reserved "IgnoredChars"
  is <- runUnlined $ some pGrapheme
  someSpace
  ignoredChars %= HS.union (HS.fromList is)

-- | Small parsers for the different constants we have.
--
-- TODO bail if we see a constant twice?

pConstants :: UnigramParser ()
pConstants = choice $ map pConstant cs
  where
    cs = [ "GapLinear", "GapOpen", "GapExtend", "PrefixSuffixOpen", "PrefixSuffixExtend"
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
  when (HS.member k e) $ fail "Character already present in EqualChars!"
  return k

reserved :: TokenParsing m => IdentifierStyle m
reserved = emptyIdents { _styleReserved = rs }
  where rs = HS.fromList [ "EqualChars", "SimilarChars", "EqualScore", "SimilarScore"
                         , "IgnoredChars"
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

