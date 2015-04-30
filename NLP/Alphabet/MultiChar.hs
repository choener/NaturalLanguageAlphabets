
-- | An alphabet, where each character is a short bytestring.
--
-- Due to the overhead this incurs, we use 'ShortByteString's internally. We
-- also provide an 'Interned' instance to further reduce overhead using
-- hash-consing.

module NLP.Alphabet.MultiChar where

import           Control.DeepSeq (NFData(..))
import           Data.Function (on)
import           Data.Hashable
import           Data.Interned
import           Data.Interned.Internal (getCache)
import           Data.Stringable
import           Data.String (IsString)
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Short.Internal as BS
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.Array as A
import           Data.Typeable (Typeable)
import           Data.Data (Data)



-- * 'MultiChar's capture UTF characters that are encoded using one or more
-- symbols.

-- | Interns a 'MultiChar' character.

internMultiChar :: MultiChar -> MultiChar
internMultiChar = uninternMultiChar . intern
{-# Inline internMultiChar #-}

-- | Wrap a short bytestring. Read and Show instances behave like for normal
-- strings.

newtype MultiChar = MultiChar { getMultiChar :: T.Text }
  deriving (Eq,Ord,Generic,Data,Typeable)

instance Show MultiChar where
  showsPrec p (MultiChar mc) r = showsPrec p (toString mc) r
  {-# Inline showsPrec #-}

instance Read MultiChar where
  readsPrec p str = [ (MultiChar x, y) | (x,y) <- readsPrec p str ]
  {-# Inline readsPrec #-}

instance Hashable MultiChar

instance IsString MultiChar where
  fromString = MultiChar . S.fromString
  {-# Inline fromString #-}

instance Stringable MultiChar where
  toString   = T.unpack . getMultiChar
  fromString = MultiChar . T.pack
  length     = T.length . getMultiChar
  fromText   = MultiChar
  toText     = getMultiChar
  {-# Inline toString   #-}
  {-# Inline fromString #-}
  {-# Inline length     #-}
  {-# Inline fromText   #-}
  {-# Inline toText     #-}

instance NFData MultiChar where
  rnf = rnf . getMultiChar
  {-# Inline rnf #-}



-- * Interned

-- | Interned 'MultiChar'.
--
-- TODO Check 'Ord' instance. We @compare `on` uninternMultiChar@.

data InternedMultiChar = InternedMultiChar
  { internedMultiCharId :: {-# UNPACK #-} !Id
  , uninternMultiChar   :: {-# UNPACK #-} !MultiChar
  }
  deriving (Generic,Data,Typeable)

instance IsString InternedMultiChar where
  fromString = intern . S.fromString
  {-# Inline fromString #-}

instance Eq InternedMultiChar where
  (==) = (==) `on` internedMultiCharId
  {-# Inline (==) #-}

instance Ord InternedMultiChar where
  compare = compare `on` uninternMultiChar -- internedMultiCharId
  {-# Inline compare #-}

instance Read InternedMultiChar where
  readsPrec p str = [ (intern x, y) | (x,y) <- readsPrec p str ]
  {-# Inline readsPrec #-}

instance Show InternedMultiChar where
  showsPrec d (InternedMultiChar _ mc) = showsPrec d mc
  {-# Inline showsPrec #-}

instance Hashable InternedMultiChar where
  hashWithSalt salt = hashWithSalt salt . internedMultiCharId
  hash              = hash . internedMultiCharId
  {-# Inline hashWithSalt #-}
  {-# Inline hash         #-}

instance Interned InternedMultiChar where
  type Uninterned InternedMultiChar = MultiChar
  newtype Description InternedMultiChar = DMC MultiChar deriving (Eq,Hashable)
  describe = DMC -- . MultiChar . T.copy . getMultiChar -- @DMC@ alone is type-correct. With 'T.copy' we make sure not to keep long @Text@s. TODO benchmark!
  identify = InternedMultiChar
  cache    = imcCache
  {-# Inline describe #-}
  {-# Inline identify #-}
  {-# Inline cache    #-}

imcCache :: Cache InternedMultiChar
imcCache = mkCache
{-# NOINLINE imcCache #-}

instance Stringable InternedMultiChar where
  toString   = toString . uninternMultiChar
  fromString = intern . fromString
  length     = Data.Stringable.length . uninternMultiChar
  toText     = toText . uninternMultiChar
  fromText   = intern . fromText
  {-# Inline toString   #-}
  {-# Inline fromString #-}
  {-# Inline length     #-}
  {-# Inline toText     #-}
  {-# Inline fromText   #-}

instance NFData InternedMultiChar where
  rnf (InternedMultiChar i c) = rnf i `seq` rnf c
  {-# Inline rnf #-}

