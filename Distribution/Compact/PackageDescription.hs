{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE StandaloneDeriving #-}
module Distribution.Compact.PackageDescription 
    ( module Distribution.Compact.PackageDescription 
    , BuildType(..), TestType(..), BenchmarkType(..), RepoType(..)
    , RepoKind(..), BenchmarkInterface(..)
    , License(..), CompilerFlavor(..), OS(..), Arch(..)
    ) where

import Control.Monad (MonadPlus(..))
import Data.Monoid (Monoid(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Set as S

import Distribution.Compact.Package
import Distribution.Compact.ModuleName
import Distribution.Compact.FilePath

import Language.Haskell.Extension (Language(..), Extension(..), KnownExtension(..))
import Distribution.System (OS(..), Arch(..))
import Distribution.License (License(..))
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Version ( Version(..), VersionRange(..) -- TODO: replace Version
                            , anyVersion, thisVersion
                            , notThisVersion, simplifyVersionRange 
                            )
import Distribution.PackageDescription ( BuildType(..), TestType(..)
                                       , BenchmarkType(..), BenchmarkInterface(..)
                                       , RepoKind(..), RepoType(..)
                                       , CondTree(..), Condition(..)
                                       )
import qualified Distribution.PackageDescription as Slow

deriving instance Ord Extension
deriving instance Ord KnownExtension
deriving instance Ord Language


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



data Library = Library { exposedModules :: !(Vector ModuleName)
                       , libExposed     :: !Bool -- ^ Is the lib to be exposed by default?
                       , libBuildInfo   :: !BuildInfo
                       } deriving (Show, Eq, Read)

instance Monoid Library where
    mempty = Library { exposedModules = mempty
                     , libExposed     = True
                     , libBuildInfo   = mempty
                     }
    mappend a b = Library { exposedModules = combine exposedModules
                          , libExposed     = libExposed a && libExposed b -- so False propagates
                          , libBuildInfo   = combine libBuildInfo
                          }
      where combine field = field a `mappend` field b

emptyLibrary :: Library
emptyLibrary = mempty

hasLibs :: PackageDescription -> Bool
hasLibs p = maybe False (buildable . libBuildInfo) (library p)





data Executable = Executable { exeName    :: !Text
                             , modulePath :: !FastFilePath
                             , buildInfo  :: !BuildInfo
                             } deriving (Show, Read, Eq)

instance Monoid Executable where
    mempty = Executable { exeName    = mempty
                        , modulePath = mempty
                        , buildInfo  = mempty
                        }
    mappend a b = Executable { exeName    = combine' exeName
                             , modulePath = combine modulePath
                             , buildInfo  = combine buildInfo
                             }
      where combine field = field a `mappend` field b
            combine' field = case (T.null $ field a, T.null $ field b) of
                      (True, True) -> T.empty
                      (True, False) -> x
                      (False, True) -> y
                      (False, False) -> error $ "Ambiguous values for executable field: '"
                                                ++ T.unpack x ++ "' and '" 
                                                ++ T.unpack y ++ "'"
              where x = field a
                    y = field b

emptyExecutable :: Executable
emptyExecutable = mempty

hasExes :: PackageDescription -> Bool
hasExes p = V.any (buildable . buildInfo) (executables p)



data TestSuite = TestSuite { testName      :: !Text
                           , testInterface :: !TestSuiteInterface
                           , testBuildInfo :: !BuildInfo
                           , testEnabled   :: !Bool
                           } deriving (Show, Read, Eq)

data TestSuiteInterface =

     -- | Test interface \"exitcode-stdio-1.0\". The test-suite takes the form
     -- of an executable. It returns a zero exit code for success, non-zero for
     -- failure. The stdout and stderr channels may be logged. It takes no
     -- command line parameters and nothing on stdin.
     --
     TestSuiteExeV10 !Version !FastFilePath

     -- | Test interface \"detailed-0.9\". The test-suite takes the form of a
     -- library containing a designated module that exports \"tests :: [Test]\".
     --
   | TestSuiteLibV09 !Version !ModuleName

     -- | A test suite that does not conform to one of the above interfaces for
     -- the given reason (e.g. unknown test type).
     --
   | TestSuiteUnsupported !TestType
   deriving (Eq, Read, Show)

instance Monoid TestSuite where
    mempty = TestSuite { testName      = mempty
                       , testInterface = mempty
                       , testBuildInfo = mempty
                       , testEnabled   = False
                       }
    mappend a b = TestSuite { testName      = combine' testName
                            , testInterface = combine  testInterface
                            , testBuildInfo = combine  testBuildInfo
                            , testEnabled   = if testEnabled a then True 
                                                               else testEnabled b
                            }
        where combine   field = field a `mappend` field b
              combine' f = case (T.null $ f a, T.null $ f b) of
                        (True, _) -> x
                        (_, True) -> y
                        (False, False) -> error $ 
                            concat [ "Ambiguous values for test field: '"
                                   , T.unpack x, "' and '", T.unpack y, "'"
                                   ]
                where x = f a
                      y = f b

instance Monoid TestSuiteInterface where
    mempty  =  TestSuiteUnsupported (TestTypeUnknown mempty (Version mempty mempty))
    mappend a (TestSuiteUnsupported _) = a
    mappend _ b                        = b


data Benchmark = Benchmark { benchmarkName      :: !Text
                           , benchmarkInterface :: !BenchmarkInterface
                           , benchmarkBuildInfo :: !BuildInfo
                           , benchmarkEnabled   :: !Bool
                           } deriving (Show, Read, Eq)




instance Monoid Benchmark where
    mempty = Benchmark { benchmarkName      = mempty
                       , benchmarkInterface = mempty
                       , benchmarkBuildInfo = mempty
                       , benchmarkEnabled   = False
                       }
    mappend a b = Benchmark { benchmarkName      = combine' benchmarkName
                            , benchmarkInterface = combine  benchmarkInterface
                            , benchmarkBuildInfo = combine  benchmarkBuildInfo
                            , benchmarkEnabled   = if benchmarkEnabled a then True
                                                                            else benchmarkEnabled b
                            }
        where combine   field = field a `mappend` field b
              combine' f = case (T.null $ f a, T.null $ f b) of
                        (True, _) -> x
                        (_, True) -> y
                        (False, False) -> error $ 
                            concat [ "Ambiguous values for benchmark field: '"
                                   , T.unpack x, "' and '", T.unpack y, "'"
                                   ]
                where x = f a
                      y = f b


data BuildInfo = BuildInfo 
    { buildable         :: !Bool                  -- ^ component is buildable here
    , buildTools        :: !(Vector Dependency)   -- ^ tools needed to build this bit
    , cppOptions        :: !(Vector Text)         -- ^ options for pre-processing Haskell code
    , ccOptions         :: !(Vector Text)         -- ^ options for C compiler
    , ldOptions         :: !(Vector Text)         -- ^ options for linker
    , pkgconfigDepends  :: !(Vector Dependency)   -- ^ pkg-config packages that are used
    , frameworks        :: !(Vector Text)         -- ^support frameworks for Mac OS X
    , cSources          :: !(Vector FastFilePath)
    , hsSourceDirs      :: !(Vector FastFilePath) -- ^ where to look for the haskell module hierarchy
    , otherModules      :: !(Vector ModuleName)   -- ^ non-exposed or non-main modules
    , defaultLanguage   :: !(Maybe Language)      -- ^ language used when not explicitly specified
    , otherLanguages    :: !(Vector Language)     -- ^ other languages used within the package
    , defaultExtensions :: !(Vector Extension)    -- ^ language extensions used by all modules
    , otherExtensions   :: !(Vector Extension)    -- ^ other language extensions used within the package
    , oldExtensions     :: !(Vector Extension)    -- ^ the old extensions field, treated same as 'defaultExtensions'
    , extraLibs         :: !(Vector Text)         -- ^ what libraries to link with when compiling a program that uses your package
    , extraLibDirs      :: !(Vector Text)
    , includeDirs       :: !(Vector FastFilePath) -- ^directories to find .h files
    , includes          :: !(Vector FastFilePath) -- ^ The .h files to be found in includeDirs
    , installIncludes   :: !(Vector FastFilePath) -- ^ .h files to install with the package
    , options           :: !(Vector (CompilerFlavor, Vector Text))
    , ghcProfOptions    :: !(Vector Text)
    , ghcSharedOptions  :: !(Vector Text)
    , customFieldsBI    :: !(Vector (Text, Text)) -- ^Custom fields starting
                                                  -- with x-, stored in a
                                                  -- simple assoc-list.
    , targetBuildDepends :: !(Vector Dependency) -- ^ Dependencies specific to a library or executable target
    } deriving (Show,Read,Eq)

instance Monoid BuildInfo where
  mempty = BuildInfo 
    { buildable         = True
    , buildTools        = V.empty
    , cppOptions        = V.empty
    , ccOptions         = V.empty
    , ldOptions         = V.empty
    , pkgconfigDepends  = V.empty
    , frameworks        = V.empty
    , cSources          = V.empty
    , hsSourceDirs      = V.empty
    , otherModules      = V.empty
    , defaultLanguage   = Nothing
    , otherLanguages    = V.empty
    , defaultExtensions = V.empty
    , otherExtensions   = V.empty
    , oldExtensions     = V.empty
    , extraLibs         = V.empty
    , extraLibDirs      = V.empty
    , includeDirs       = V.empty
    , includes          = V.empty
    , installIncludes   = V.empty
    , options           = V.empty
    , ghcProfOptions    = V.empty
    , ghcSharedOptions  = V.empty
    , customFieldsBI    = V.empty
    , targetBuildDepends = V.empty
    }
  mappend a b = BuildInfo {
    buildable         = buildable a && buildable b,
    buildTools        = combine    buildTools,
    cppOptions        = combine    cppOptions,
    ccOptions         = combine    ccOptions,
    ldOptions         = combine    ldOptions,
    pkgconfigDepends  = combine    pkgconfigDepends,
    frameworks        = combineNub frameworks,
    cSources          = combineNub cSources,
    hsSourceDirs      = combineNub hsSourceDirs,
    otherModules      = combineNub otherModules,
    defaultLanguage   = combineMby defaultLanguage,
    otherLanguages    = combineNub otherLanguages,
    defaultExtensions = combineNub defaultExtensions,
    otherExtensions   = combineNub otherExtensions,
    oldExtensions     = combineNub oldExtensions,
    extraLibs         = combine    extraLibs,
    extraLibDirs      = combineNub extraLibDirs,
    includeDirs       = combineNub includeDirs,
    includes          = combineNub includes,
    installIncludes   = combineNub installIncludes,
    options           = combine    options,
    ghcProfOptions    = combine    ghcProfOptions,
    ghcSharedOptions  = combine    ghcSharedOptions,
    customFieldsBI    = combine    customFieldsBI,
    targetBuildDepends = combineNub targetBuildDepends
  }
    where combine    field = field a `mappend` field b
          combineNub field = nubV (combine field)
          combineMby field = field b `mplus` field a
          -- TODO: this is probably terrible
          nubV :: (Ord a) => Vector a -> Vector a
          nubV = V.fromList . S.toList . S.fromList . V.toList
      


type HookedBuildInfo = (Maybe BuildInfo, [(String, BuildInfo)])




data SourceRepo = SourceRepo 
    { repoKind     :: !RepoKind
    , repoType     :: !(Maybe RepoType)
    , repoLocation :: !(Maybe Text)
    , repoModule   :: !(Maybe Text)
    , repoBranch   :: !(Maybe Text)
    , repoTag      :: !(Maybe Text)
    , repoSubdir   :: !(Maybe FastFilePath)
    } deriving (Eq, Read, Show)



data GenericPackageDescription = GenericPackageDescription
    { packageDescription :: !PackageDescription
    , genPackageFlags    :: !(Vector Flag)
    , condLibrary        :: !(Maybe (DepCondTree Library))
    , condExecutables    :: !(Vector (Text, DepCondTree Executable))
    , condTestSuites     :: !(Vector (Text, DepCondTree TestSuite))
    , condBenchmarks     :: !(Vector (Text, DepCondTree Benchmark))
    } deriving (Show, Eq)

type DepCondTree = CondTree ConfVar [Dependency]

instance Package GenericPackageDescription where
  packageId = packageId . packageDescription



data Flag = MkFlag
    { flagName        :: !FlagName
    , flagDescription :: !Text
    , flagDefault     :: !Bool
    , flagManual      :: !Bool
    }
    deriving (Show, Eq)

newtype FlagName = FlagName { getFlagName :: Text }
    deriving (Eq, Ord, Show, Read)

type FlagAssignment = [(FlagName, Bool)]

data ConfVar = OS !OS
             | Arch !Arch
             | Flag !FlagName
             | Impl !CompilerFlavor !VersionRange
    deriving (Eq, Show)


