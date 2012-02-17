{-# OPTIONS_GHC -funbox-strict-fields #-}
module Distribution.Compact.ModuleName where

import System.FilePath
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

newtype ModuleName = ModuleName { components :: Vector Text }
    deriving (Eq, Ord, Read, Show)

simple :: Text -> ModuleName
simple = ModuleName . V.singleton

main :: ModuleName
main = simple . T.pack $ "Main"

toFilePath :: ModuleName -> FilePath
toFilePath = concat 
           . intersperse [pathSeparator] 
           . map T.unpack
           . V.toList . components
