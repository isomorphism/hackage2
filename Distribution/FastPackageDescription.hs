{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Space-optimised variants of the types in
-- "Distribution.PackageDescription". Should ideally be merged upstream.
module Distribution.FastPackageDescription
    ( FastFilePath(..)
    , PackageName(..)
    , PackageIdentifier(..)
    , PackageId
    , Package(..)
    , PackageDescription(..)
    , Dependency(..)
    , fromSlow
    , toSlow
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Distribution.Version (Version, VersionRange)
import Distribution.License (License)
import Distribution.Compiler (CompilerFlavor)
-- It would be nice if we didn't rely on SourceRepo, Library, Executable, TestSuite
-- and Benchmark. They contain Strings (SourceRepo has a *lot* of them), and
-- having to deal with multiple types of FilePath is probably going to be
-- really annoying.
import Distribution.PackageDescription
    (BuildType, SourceRepo, Library, Executable, TestSuite, Benchmark)
import qualified Distribution.PackageDescription as Slow

-- | Ideally this would be System.Posix.ByteString.RawFilePath, but that would
-- introduce a dependency on unix >= 2.5.1.0. Text is used here for consistency
-- with e.g. the URL field in PackageDescription.
newtype FastFilePath = FastFilePath { getFastFilePath :: Text }
    deriving (Eq, Ord, Read, Show)

newtype PackageName = PackageName { getPackageName :: Text }
    deriving (Eq, Ord, Read, Show)

data PackageIdentifier = PackageIdentifier
    { pkgName    :: !PackageName
    , pkgVersion :: !Version  -- perhaps replace Version?
    } deriving (Eq, Ord, Read, Show)

type PackageId = PackageIdentifier

class Package pkg where
    packageId :: pkg -> PackageIdentifier

instance Package PackageIdentifier where
    packageId = id

-- | This omits the @dataFiles@, @dataDir@, @extraSrcFiles@ and @extraTmpFiles@
-- fields of the original 'Slow.PackageDescription', since they're not very
-- useful for us.
data PackageDescription = PackageDescription
    { package        :: !PackageIdentifier
    , license        :: !License
    , licenseFile    :: !FastFilePath
    , copyright      :: !Text
    , maintainer     :: !Text
    , author         :: !Text
    , stability      :: !Text
    , testedWith     :: !(Vector (CompilerFlavor, VersionRange))
    , homepage       :: !Text
    , pkgUrl         :: !Text
    , bugReports     :: !Text
    , sourceRepos    :: !(Vector SourceRepo)
    , synopsis       :: !Text
    , description    :: !Text
    , category       :: !Text
    , customFieldsPD :: !(Vector (Text, Text))
    , buildDepends   :: !(Vector Dependency)
    , specVersionRaw :: !(Either Version VersionRange)
    , buildType      :: !(Maybe BuildType)
    , library        :: !(Maybe Library)
    , executables    :: !(Vector Executable)
    , testSuites     :: !(Vector TestSuite)
    , benchmarks     :: !(Vector Benchmark)
    } deriving (Eq, Read, Show)

instance Package PackageDescription where
    packageId = package

data Dependency = Dependency !PackageName !VersionRange
    deriving (Eq, Read, Show)

{-# WARNING fromSlow, toSlow "STOP! These have not yet been implemented." #-}

fromSlow :: Slow.PackageDescription -> PackageDescription
fromSlow = error "fromSlow: not yet implemented"

-- | Necessarily lossy, as our version omits some fields.
toSlow :: PackageDescription -> Slow.PackageDescription
toSlow = error "toSlow: not yet implemenetd"
