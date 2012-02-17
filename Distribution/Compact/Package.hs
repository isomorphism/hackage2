{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE StandaloneDeriving #-}
module Distribution.Compact.Package where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Distribution.Version ( Version(..), VersionRange(..)
                            , anyVersion, thisVersion
                            , notThisVersion, simplifyVersionRange 
                            )

deriving instance Ord VersionRange

newtype PackageName = PackageName { getPackageName :: Text }
    deriving (Eq, Ord, Read, Show)


data PackageIdentifier = PackageIdentifier
    { pkgName    :: !PackageName
    , pkgVersion :: !Version  -- perhaps replace Version?
    } deriving (Eq, Ord, Read, Show)

type PackageId = PackageIdentifier

newtype InstalledPackageId = InstalledPackageId { installedPkgId :: Text } 
    deriving (Eq, Ord, Read, Show)

data Dependency = Dependency !PackageName !VersionRange
    deriving (Eq, Ord, Read, Show)




class Package pkg where
    packageId :: pkg -> PackageIdentifier

instance Package PackageIdentifier where
    packageId = id

packageName    :: Package pkg => pkg -> PackageName
packageName     = pkgName    . packageId

packageVersion :: Package pkg => pkg -> Version
packageVersion  = pkgVersion . packageId


class (Package pkg) => PackageFixedDeps pkg where
    depends :: pkg -> [PackageIdentifier]

