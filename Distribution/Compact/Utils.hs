{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Compact.Utils where

import Data.Monoid (Monoid(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Distribution.Simple.Utils as U

import Distribution.Compact.FilePath

currentDir = FastFilePath . T.pack $ U.currentDir
