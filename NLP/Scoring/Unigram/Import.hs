
-- |
--
-- TODO normalization of characters!

module NLP.Scoring.Unigram.Import where

import           Control.Applicative
import           Control.Arrow (first, (***))
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Class
import           Control.Monad.Trans.State.Strict hiding (gets)
import           Control.Monad.Except
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.HashMap.Strict (fromList, HashMap)
import           Data.HashSet (HashSet)
import           Data.Maybe
import           Data.Monoid
import           Data.String (IsString)
import           Data.Text (Text)
import           Debug.Trace
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Exit (exitFailure)
import           System.IO (stdout)
import           Text.Parser.LookAhead
import           Text.Parser.Token.Style
import           Text.PrettyPrint.ANSI.Leijen (displayIO, renderPretty, linebreak, displayS)
import           Text.Trifecta as TT
import           Text.Trifecta.Delta (Delta(..))

import           Data.ByteString.Interned

import           NLP.Scoring.Unigram



data Env = Env
  { _warnings           :: S.Seq Text
  , _defaults           :: HashMap Text Double
  , _charGroups         :: HashMap Text (HashSet Text)
  , _matchScores        :: HashMap (Text,Text) Double
  , _ignoredScoresFstK  :: HashMap Text Double
  , _ignoredScoresSndL  :: HashMap Text Double
  }
  deriving (Show)

makeLenses ''Env

defaultEnv = Env
  { _warnings           = S.empty
  , _defaults           = HM.empty
  , _charGroups         = HM.empty
  , _matchScores        = HM.empty
  , _ignoredScoresFstK  = HM.empty
  , _ignoredScoresSndL  = HM.empty
  }



test = fromFile True "scoring/unigramdefault.score"

-- | This will prettyprint the error message and ungracefully exit

prettyErrorAndExit :: MonadIO m => ErrInfo -> m ()
prettyErrorAndExit e = do
  liftIO $ displayIO stdout $ renderPretty 0.8 80 $ (_errDoc e) <> linebreak
  liftIO $ exitFailure

-- | Returns the error message, but will not exit.

errorToString :: ErrInfo -> String
errorToString e = (displayS . renderPretty 0.8 80 $ _errDoc e) ""

fromByteString :: ByteString -> String -> Except ErrInfo (UnigramScoring k l)
fromByteString s fn = r where
  p = parseByteString ((runStateT . runUnigramParser) pUnigram defaultEnv)
                      (Directed (UTF8.fromString fn) 0 0 0 0) s
  r = case p of
        Success (p',e) -> return p'
        Failure e      -> throwError e

fromFile :: Bool -> FilePath -> ExceptT ErrInfo IO (UnigramScoring k l)
fromFile warn fp = do
  p' <- TT.parseFromFileEx ((runStateT . runUnigramParser) pUnigram defaultEnv) fp
  case p' of
    Success (p,e) -> do
      let ws = e^.warnings
      unless (null ws || not warn) $ do
        liftIO $ mapM_ T.putStrLn ws
      return p
    Failure e -> throwError e

pUnigram :: UnigramParser (UnigramScoring k l)
pUnigram = do
  skipOptional someSpace
  many $ choice [pDefaults, pCharGroup, pSimilarity, pEquality, pIgnored]
  eof
  let uconstants :: Text -> UnigramParser Double
      uconstants k = do
        kv <- use defaults
        case HM.lookup k kv of
          Nothing -> do
            warnings %= (S.|> ("constant " <> k <> " not found, using default (-999999)"))
            -- fail $ "constant " <> show k <> " not found"
            return (-999999)
          Just v  -> return v
  usUnigramMatch      <- (HM.fromList . map (first (ibsText *** ibsText)) . HM.toList) <$> use matchScores
  usUnigramInsertFstK <- (HM.fromList . map (first ibsText) . HM.toList) <$> use ignoredScoresFstK
  usUnigramInsertSndL <- (HM.fromList . map (first ibsText) . HM.toList) <$> use ignoredScoresSndL
  usGapLinear     <- uconstants "GapLinear"
  usGapOpen       <- uconstants "GapOpen"
  usGapExtension  <- uconstants "GapExtension"
  usDefaultMatch    <- uconstants "Match"
  usDefaultMismatch <- uconstants "Mismatch"
  usPrefixSuffixLinear    <- uconstants "PrefixSuffixLinear"
  usPrefixSuffixOpen      <- uconstants "PrefixSuffixOpen"
  usPrefixSuffixExtension <- uconstants "PrefixSuffixExtension"
--  -- Given the @Env@, we can now construct the actual scoring system.
  return UnigramScoring{..}

-- | Defaults are key-value pairs, of which there is only a small set.

pDefaults :: UnigramParser ()
pDefaults = choice $ map pConstant cs
  where
    cs = [ "GapLinear", "GapOpen", "GapExtend", "PrefixSuffixOpen", "PrefixSuffixExtend"
         , "Match", "Mismatch"
         ]
    pConstant :: Text -> UnigramParser ()
    pConstant r = do
      reserveText reserved r
      ds <- use defaults
      when (HM.member r ds) (fail $ show r ++ " already defined")
      v <- either fromIntegral id <$> integerOrDouble
      defaults %= HM.insert r v

-- | Gives a name to a set of characters we want to work with later on.

pCharGroup :: UnigramParser ()
pCharGroup = do
  reserve reserved "CharGroup"
  ty <- ident reserved
  -- TODO the {,} guys
  ls <- option [] . braces $ pExpansionOptions `sepEndBy` comma
  vs' <- runUnlined $ do
    gs <- HS.unions <$> (some $ (HS.singleton <$> pGrapheme) <|> pKnownCharGroup)
    rol <- restOfLine
    unless (rol == "\n") $ fail $ show (gs,rol)
    return gs
  someSpace
  let vs = applySpecialFunctions ls vs'
  charGroups %= HM.insert ty vs

-- | Parses a similarity line and updates the scores for the pairs of
-- characters.

pSimilarity :: UnigramParser ()
pSimilarity = do
  reserve reserved "Similarity"
  ls1 <- option [] . braces $ pExpansionOptions `sepEndBy` comma
  ty1 <- runUnlined pKnownCharGroup
  ls2 <- option [] . braces $ pExpansionOptions `sepEndBy` comma
  ty2 <- runUnlined pKnownCharGroup
  v  <- either fromIntegral id <$> integerOrDouble
  let xs = applySpecialFunctions ls1 ty1
  let ys = applySpecialFunctions ls2 ty2
  let vs = HM.fromList [ ((x,y),v) | x <- HS.toList xs, y <- HS.toList ys ]
  -- mapping from the first will be the result in clashes, hence @HS.union
  -- vs old@ ...
  matchScores %= HM.union vs

-- | Parses an equality line and updates the scores for the pairs of
-- characters.

pEquality :: UnigramParser ()
pEquality = do
  reserve reserved "Equality"
  ls <- option [] . braces $ pExpansionOptions `sepEndBy` comma
  ty <- runUnlined pKnownCharGroup
  v  <- either fromIntegral id <$> integerOrDouble
  let xss = map (applySpecialFunctions ls . HS.singleton) $ HS.toList ty
  let vs = HM.fromList [ ((x,y),v) | xs <- xss, x <- HS.toList xs, y <- HS.toList xs ]
  matchScores %= HM.union vs

data FstKSndL = FstK | SndL
  deriving (Eq,Ord)

pIgnored :: UnigramParser ()
pIgnored =   (reserve reserved "Ignored"    >> go [FstK,SndL])
         <|> (reserve reserved "IgnoredFst" >> go [FstK]     )
         <|> (reserve reserved "IgnoredSnd" >> go [     SndL])
  where
    go what = do
      ls <- option [] . braces $ pExpansionOptions `sepEndBy` comma
      ty <- runUnlined pKnownCharGroup
      v  <- either fromIntegral id <$> integerOrDouble
      let xs = applySpecialFunctions ls ty
      let vs = HM.fromList [ (x,v) | x <- HS.toList xs ]
      when (FstK `elem` what) $ ignoredScoresFstK %= HM.union vs
      when (SndL `elem` what) $ ignoredScoresSndL %= HM.union vs

-- | Defines what a grapheme is. Basically, don't be a whitespace and don't
-- start with '$'.
--
-- TODO we probably want to allow \$ to stand for '$'.

pGrapheme :: (CharParsing p, TokenParsing p) => p Text
pGrapheme = (T.pack <$> some (satisfy allowed) <* someSpace) <?> "pGrapheme"
  where allowed x = (not $ isSpace x || x `elem` ("${}" :: String))

-- | Returns the set of characters from a known character group

pKnownCharGroup :: Unlined UnigramParser (HS.HashSet Text)
pKnownCharGroup = go <?> "pKnownCharGroup" where
  go = do
    char '$'
    ty <- ident reserved
    cgs <- use charGroups
    case HM.lookup ty cgs of
      Nothing -> fail $ show ty ++ " is not a known CharGroup!"
      Just cg -> return cg

-- | How we can expand a group with special functions.

pExpansionOptions :: UnigramParser Text
pExpansionOptions = choice $ map (text . fst) specialFunctions

specialFunctions ∷ [(Text, Text → Text)]
specialFunctions =
  [ ("ToUpper", T.toUpper)
  , ("ToLower", T.toLower)
  ]

applySpecialFunctions ls xs =
  HS.unions $ xs : [ HS.map sf xs | (sfn,sf) <- specialFunctions, sfn `elem` ls ]
{-
-- | Read a line containing an "EqualChars". Will only parse successfully if the
-- set is not yet known. Inserts the set into the @Env@.

pEqualChars :: UnigramParser ()
pEqualChars = do
  ks <- uses equalScores HM.keys
  reserve reserved "EqualChars"
  ls <- option [] . braces $ pExpansionOptions `sepEndBy` comma
  when (null ks) $ fail "no EqualChars's defined!"
  ty <- choice $ map (\t -> runUnspaced (textSymbol t) <* someSpace) ks
  vs <- runUnlined $ some pGrapheme
  someSpace
  -- handle expansion options
  let vsFinal =  vs
              ++ [ T.toUpper v | "ToUpper" `elem` ls, v <- vs ]
              ++ [ T.toLower v | "ToLower" `elem` ls, v <- vs ]
  equalChars %= (HS.insert (ty, HS.fromList vsFinal))

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

{-
pChoice :: UnigramParser ()
pChoice = choice $ map pOneChoice cs
  where
    cs = [ "IgnoreCase"
         ]
    pOneChoice :: Text -> UnigramParser ()
    pOneChoice r = do
      reserveText reserved r
      tf <- option True $ (True <$ textSymbol "True") <|> (False <$ textSymbol "False")
      when tf $ choices %= HS.insert r
-}

-}

-- | TODO only insert warning, not error, after seeing a character again!

setIdent :: HashSet Text -> Unlined UnigramParser Text
setIdent e = try $ do
  k <- ident reserved
  when (HS.member k e) $ fail "Character already present in EqualChars!"
  return k

reserved :: TokenParsing m => IdentifierStyle m
reserved = emptyIdents { _styleReserved = rs }
  where rs = HS.fromList [ -- "EqualChars", "SimilarChars", "EqualScore", "SimilarScore"
                         -- , "IgnoredChars", "CharGroup"
                         ]


newtype UnigramParser a = UnigramParser { runUnigramParser :: StateT Env Parser a }
  deriving
    ( Alternative
    , Applicative
    , CharParsing
    , DeltaParsing
    , Functor
    , Monad
    , MonadPlus
    , MonadState Env
    , Parsing
    , LookAheadParsing
    )

instance TokenParsing UnigramParser where
  someSpace = buildSomeSpaceParser (() <$ space) haskellCommentStyle

deriving instance DeltaParsing (Unlined UnigramParser)

