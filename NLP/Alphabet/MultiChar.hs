{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | An alphabet, where each character is a short bytestring.
--
-- Due to the overhead this incurs, we use 'ShortByteString's internally. We
-- also provide an 'Interned' instance to further reduce overhead using
-- hash-consing.
--
-- TODO we'd like to use the @stringable@ library but it depends on
-- @system-filepath@ which is not yet compatible with @text>=1@.

module NLP.Alphabet.MultiChar where

import           Data.Function (on)
import           Data.Hashable
import           Data.Interned
import           Data.Stringable
import           Data.String (IsString)
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Short.Internal as BS
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T



-- | Interns a 'MultiChar' character.

internMultiChar :: MultiChar -> MultiChar
internMultiChar = uninternMultiChar . intern

-- | Wrap a short bytestring. Read and Show instances behave like for normal
-- strings.

newtype MultiChar = MultiChar { unMultiChar :: BS.ShortByteString }
  deriving (Eq,Ord)

instance Show MultiChar where
  showsPrec p (MultiChar ps) r = showsPrec p ps r

instance Read MultiChar where
  readsPrec p str = [ (MultiChar x, y) | (x,y) <- readsPrec p str ]

instance Hashable MultiChar where
  hashWithSalt salt (MultiChar s@(BS.SBS sbs)) = hashByteArrayWithSalt sbs 0 (BS.length s) salt

instance IsString MultiChar where
  fromString = MultiChar . S.fromString

instance Stringable MultiChar where
  toString   = T.unpack . T.decodeUtf8 . BS.fromShort . unMultiChar
  fromString = MultiChar . BS.toShort . T.encodeUtf8 . T.pack
  length     = BS.length . unMultiChar



-- * Interned

data InternedMultiChar = InternedMultiChar
  { internedMultiCharId :: {-# UNPACK #-} !Id
  , uninternMultiChar   :: {-# UNPACK #-} !MultiChar
  }

instance IsString InternedMultiChar where
  fromString = intern . S.fromString

instance Eq InternedMultiChar where
  (==) = (==) `on` internedMultiCharId

instance Ord InternedMultiChar where
  compare = compare `on` internedMultiCharId

instance Show InternedMultiChar where
  showsPrec d (InternedMultiChar _ mc) = showsPrec d mc

instance Interned InternedMultiChar where
  type Uninterned InternedMultiChar = MultiChar
  newtype Description InternedMultiChar = DMC MultiChar deriving (Eq,Hashable)
  describe = DMC
  identify = InternedMultiChar
  cache = imcCache

imcCache :: Cache InternedMultiChar
imcCache = mkCache
{-# NOINLINE imcCache #-}

instance Stringable InternedMultiChar where
  toString   = toString . uninternMultiChar
  fromString = intern . fromString
  length     = Data.Stringable.length . uninternMultiChar

