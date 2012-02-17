{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Compact.FilePath where

import Data.Monoid (Monoid(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V


-- | Ideally this would be System.Posix.ByteString.RawFilePath, but that would
-- introduce a dependency on unix >= 2.5.1.0. Text is used here for consistency
-- with e.g. the URL field in PackageDescription.
newtype FastFilePath = FastFilePath { getFastFilePath :: Text }
    deriving (Eq, Ord, Read, Show, Monoid)

