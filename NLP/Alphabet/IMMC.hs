
-- | An implementation of @Int@-mapped @MultiChar@s with internalization.

module NLP.Alphabet.IMMC where

import           Control.DeepSeq (NFData(..))
import           Data.Hashable
import           Data.Stringable as SA
import           Data.String as IS
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import qualified Data.ByteString.Short as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           NLP.Alphabet.IMMC.Internal
import           NLP.Alphabet.MultiChar (InternedMultiChar)



-- * A somewhat fragile (?) encoding of multichars using the internalized
-- @Id@. Should only be used via it's wrapper. The mapped @Int@s are not
-- consecutive.

newtype IMMC = IMMC { getIMMC :: Int }
  deriving (Eq,Generic)

derivingUnbox "IMMC"
  [t| IMMC -> Int |]
  [|  getIMMC     |]
  [|  IMMC        |]

instance Ord IMMC where
  IMMC l `compare` IMMC r = immcBimapLookupInt l `compare` immcBimapLookupInt r
  {-# Inline compare #-}

immc :: InternedMultiChar -> IMMC
immc s = IMMC $! immcBimapAdd s
{-# Inline immc #-}

instance IsString IMMC where
  fromString = immc . IS.fromString
  {-# Inline fromString #-}

instance Show IMMC where
  showsPrec p i r = showsPrec p (toString i) r
  {-# Inline showsPrec #-}

instance Read IMMC where
  readsPrec p str = [ (immc $ IS.fromString s, y) | (s,y) <- readsPrec p str ]
  {-# Inline readsPrec #-}

instance Hashable IMMC

instance Stringable IMMC where
  toString   = toString . immcBimapLookupInt . getIMMC
  fromString = immc . SA.fromString
  length     = SA.length . immcBimapLookupInt . getIMMC
  toText     = toText . immcBimapLookupInt . getIMMC
  fromText   = immc . fromText
  {-# Inline toString   #-}
  {-# Inline fromString #-}
  {-# Inline length     #-}
  {-# Inline toText     #-}
  {-# Inline fromText   #-}

instance NFData IMMC where
  rnf = rnf . getIMMC
  {-# Inline rnf #-}

