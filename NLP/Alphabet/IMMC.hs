
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

immc s = IMMC $! immcBimapAdd s
{-# Inline immc #-}

instance IsString IMMC where
  fromString = immc . IS.fromString
  {-# Inline fromString #-}

instance Show IMMC where
  showsPrec p (IMMC i) r = seq i $ showsPrec p (immcBimapLookupInt i) r
  {-# Inline showsPrec #-}

instance Read IMMC where
  readsPrec p str = [ (immc $ IS.fromString s, y) | (s,y) <- readsPrec p str ]
  {-# Inline readsPrec #-}

instance Hashable IMMC

instance Stringable IMMC where
  toString   (IMMC i) = seq i . toString $ immcBimapLookupInt i
  fromString          = immc . SA.fromString
  length     (IMMC i) = seq i . SA.length $ immcBimapLookupInt i
  {-# Inline toString #-}
  {-# Inline fromString #-}
  {-# Inline length #-}

instance NFData IMMC where
  rnf = rnf . getIMMC
  {-# Inline rnf #-}
