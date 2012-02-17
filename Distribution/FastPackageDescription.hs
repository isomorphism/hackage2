{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Space-optimised variants of the types in
-- "Distribution.PackageDescription". Should ideally be merged upstream.
module Distribution.FastPackageDescription
    ( FastFilePath(..), PackageName(..)
    , PackageIdentifier(..), PackageId, Package(..)
    , PackageDescription(..), GenericPackageDescription(..)
    , Dependency(..), ModuleName(..), BuildType(..), BuildInfo(..)
    , Library(..), Executable(..), TestSuite(..), Benchmark(..)
    , Flag(..), FlagName(..), FlagAssignment(..)
    , ConfVar(..), CondTree(..), Condition(..)
    , SourceRepo(..), RepoKind(..), RepoType(..)
    , packageName, packageVersion
    , hasLibs, hasExes
    , flattenPackageDescription
    , fromSlow, toSlow
    , genFromSlow
    , pkgNameFromSlow, pkgNameToSlow
    , pkgIdFromSlow, pkgIdToSlow
    ) where

import Distribution.Compact.ModuleName
import Distribution.Compact.Package
import Distribution.Compact.PackageDescription
import Distribution.Compact.FilePath
import Distribution.Compact.Utils

import Control.Applicative 
import Control.Arrow ((***), second)
import Data.Monoid (Monoid(..))
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Distribution.Text as DT
import qualified Distribution.ModuleName as DM
import Distribution.Version ( Version(..), VersionRange(..)
                            , anyVersion, thisVersion
                            , notThisVersion, simplifyVersionRange 
                            )
import Distribution.License (License(..))
import Distribution.Compiler (CompilerFlavor(..))
-- It would be nice if we didn't rely on SourceRepo, Library, Executable, TestSuite
-- and Benchmark. They contain Strings (SourceRepo has a *lot* of them), and
-- having to deal with multiple types of FilePath is probably going to be
-- really annoying.
import Distribution.PackageDescription
    ( BuildType(..), Condition(..), CondTree(..)
    , RepoKind(..), RepoType(..)
    )
import qualified Distribution.Package as Pkg
import qualified Distribution.PackageDescription as Slow

import Distribution.Text hiding (Text)
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP ((<++))
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>), (<+>), text)
import qualified Data.Char as Char ( isDigit, isAlphaNum )
import Data.List ( intersperse )


filePathFromSlow = FastFilePath . T.pack

pkgNameFromSlow (Pkg.PackageName n) = PackageName (T.pack n)
pkgNameToSlow = Pkg.PackageName . T.unpack . getPackageName

instance DT.Text PackageName where
    disp (PackageName n) = Disp.text $ T.unpack n
    parse = do ns <- Parse.sepBy1 component (Parse.char '-')
               return (PackageName (T.pack $ concat (intersperse "-" ns)))
      where component = do cs <- Parse.munch1 Char.isAlphaNum
                           if all Char.isDigit cs then Parse.pfail else return cs


pkgIdFromSlow :: Pkg.PackageIdentifier -> PackageIdentifier
pkgIdFromSlow pkg = PackageIdentifier 
    { pkgName = pkgNameFromSlow $ Pkg.pkgName pkg
    , pkgVersion = Pkg.pkgVersion pkg
    }

pkgIdToSlow :: PackageIdentifier -> Pkg.PackageIdentifier
pkgIdToSlow pkg = Pkg.PackageIdentifier 
    { Pkg.pkgName = pkgNameToSlow $ pkgName pkg
    , Pkg.pkgVersion = pkgVersion pkg
    }

instance DT.Text PackageIdentifier where
    disp (PackageIdentifier n v) = case v of
        Version [] _ -> disp n -- if no version, don't show version.
        _            -> disp n <> Disp.char '-' <> disp v
    parse = do n <- parse
               v <- (Parse.char '-' >> parse) <++ return (Version [] [])
               return (PackageIdentifier n v)


instance DT.Text Dependency where
  disp (Dependency name ver) = disp name <+> disp ver
  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return (Dependency name ver)

depFromSlow :: Pkg.Dependency -> Dependency
depFromSlow (Pkg.Dependency pn vr) = Dependency (pkgNameFromSlow pn) vr

genFromSlow :: Slow.GenericPackageDescription -> GenericPackageDescription
genFromSlow gpd = GenericPackageDescription
    { packageDescription = fromSlow $ Slow.packageDescription gpd
    , genPackageFlags = V.fromList . fmap flagFromSlow $ Slow.genPackageFlags gpd
    , condLibrary = condTreeFromSlow libFromSlow <$> Slow.condLibrary gpd
    , condExecutables = V.fromList 
                      . map (T.pack *** condTreeFromSlow exeFromSlow) 
                      $ Slow.condExecutables gpd
    , condTestSuites = V.fromList 
                     . map (T.pack *** condTreeFromSlow testsFromSlow) 
                     $ Slow.condTestSuites gpd
    , condBenchmarks = V.fromList 
                     . map (T.pack *** condTreeFromSlow benchFromSlow) 
                     $ Slow.condBenchmarks gpd
    } 

flagFromSlow :: Slow.Flag -> Flag
flagFromSlow (Slow.MkFlag nm desc df man) = MkFlag (flagNameFromSlow nm) 
                                                   (T.pack desc) 
                                                   df man

flagNameFromSlow :: Slow.FlagName -> FlagName
flagNameFromSlow (Slow.FlagName n) = FlagName $ T.pack n

confVarFromSlow :: Slow.ConfVar -> ConfVar
confVarFromSlow (Slow.OS os) = OS os
confVarFromSlow (Slow.Arch arch) = Arch arch
confVarFromSlow (Slow.Flag fn) = Flag $ flagNameFromSlow fn
confVarFromSlow (Slow.Impl cf vr) = Impl cf vr

condFromSlow :: Condition Slow.ConfVar -> Condition ConfVar
condFromSlow (Var c) = Var (confVarFromSlow c)
condFromSlow (Lit b) = Lit b
condFromSlow (CNot c) = CNot $ condFromSlow c
condFromSlow (COr c1 c2) = COr (condFromSlow c1) (condFromSlow c2)
condFromSlow (CAnd c1 c2) = CAnd (condFromSlow c1) (condFromSlow c2)


condTreeFromSlow :: (a -> b) -> CondTree Slow.ConfVar [Pkg.Dependency] a 
                 -> CondTree ConfVar [Dependency] b
condTreeFromSlow f (CondNode x ds chs) = CondNode (f x) ds' chs'
  where ds' = depFromSlow <$> ds
        chs' = compFromSlow <$> chs
        compFromSlow (v, ct1, ct2) = ( condFromSlow v
                                     , condTreeFromSlow f ct1
                                     , condTreeFromSlow f <$> ct2 )


fromSlow :: Slow.PackageDescription -> PackageDescription
fromSlow pkg = PackageDescription
    { package = pkgIdFromSlow $ Slow.package pkg
    , license = Slow.license pkg
    , licenseFile = filePathFromSlow $ Slow.licenseFile pkg
    , copyright = T.pack $ Slow.copyright pkg
    , maintainer = T.pack $ Slow.maintainer pkg
    , author = T.pack $ Slow.author pkg
    , stability = T.pack $ Slow.stability pkg
    , testedWith = V.fromList $ Slow.testedWith pkg
    , homepage = T.pack $ Slow.homepage pkg
    , pkgUrl = T.pack $ Slow.pkgUrl pkg
    , bugReports = T.pack $ Slow.bugReports pkg
    , sourceRepos = V.fromList . fmap srcRepoFromSlow $ Slow.sourceRepos pkg
    , synopsis = T.pack $ Slow.synopsis pkg
    , description = T.pack $ Slow.description pkg
    , category = T.pack $ Slow.category pkg
    , customFieldsPD = V.fromList . map (T.pack *** T.pack) 
                     . Slow.customFieldsPD $ pkg
    , buildDepends = V.fromList . map depFromSlow $ Slow.buildDepends pkg
    , specVersionRaw = Slow.specVersionRaw pkg
    , buildType = Slow.buildType pkg
    , library = libFromSlow <$> Slow.library pkg
    , executables = V.fromList . fmap exeFromSlow $ Slow.executables pkg
    , testSuites = V.fromList . fmap testsFromSlow $ Slow.testSuites pkg
    , benchmarks = V.fromList . fmap benchFromSlow $ Slow.benchmarks pkg
    }

srcRepoFromSlow :: Slow.SourceRepo -> SourceRepo
srcRepoFromSlow srcRepo = SourceRepo 
    { repoKind = Slow.repoKind srcRepo
    , repoType = Slow.repoType srcRepo
    , repoLocation = T.pack <$> Slow.repoLocation srcRepo
    , repoModule = T.pack <$> Slow.repoModule srcRepo
    , repoBranch = T.pack <$> Slow.repoBranch srcRepo
    , repoTag = T.pack <$> Slow.repoTag srcRepo
    , repoSubdir = filePathFromSlow <$> Slow.repoSubdir srcRepo
    }

libFromSlow :: Slow.Library -> Library
libFromSlow lib = Library 
    { exposedModules = V.fromList . map moduleNameFromSlow $ Slow.exposedModules lib
    , libExposed = Slow.libExposed lib
    , libBuildInfo = buildInfoFromSlow $ Slow.libBuildInfo lib
    }

moduleNameFromSlow :: DM.ModuleName -> ModuleName
moduleNameFromSlow = ModuleName . V.fromList . map T.pack . DM.components

exeFromSlow :: Slow.Executable -> Executable
exeFromSlow exe = Executable 
    { exeName = T.pack $ Slow.exeName exe
    , modulePath = filePathFromSlow $ Slow.modulePath exe
    , buildInfo = buildInfoFromSlow $ Slow.buildInfo exe
    }

testsFromSlow :: Slow.TestSuite -> TestSuite
testsFromSlow tests = TestSuite
    { testName = T.pack $ Slow.testName tests
    , testInterface = testIfaceFromSlow $ Slow.testInterface tests
    , testBuildInfo = buildInfoFromSlow $ Slow.testBuildInfo tests
    , testEnabled = Slow.testEnabled tests
    }

testIfaceFromSlow :: Slow.TestSuiteInterface -> TestSuiteInterface
testIfaceFromSlow (Slow.TestSuiteExeV10 v fp) = TestSuiteExeV10 v (filePathFromSlow fp)
testIfaceFromSlow (Slow.TestSuiteLibV09 v mn) = TestSuiteLibV09 v (moduleNameFromSlow mn)
testIfaceFromSlow (Slow.TestSuiteUnsupported t) = TestSuiteUnsupported t

benchFromSlow :: Slow.Benchmark -> Benchmark
benchFromSlow bench = Benchmark
    { benchmarkName = T.pack $ Slow.benchmarkName bench
    , benchmarkInterface = Slow.benchmarkInterface bench
    , benchmarkBuildInfo = buildInfoFromSlow $ Slow.benchmarkBuildInfo bench
    , benchmarkEnabled = Slow.benchmarkEnabled bench
    }

buildInfoFromSlow :: Slow.BuildInfo -> BuildInfo
buildInfoFromSlow bi = BuildInfo 
    { buildable = Slow.buildable bi
    , buildTools = V.fromList . map depFromSlow $ Slow.buildTools bi
    , cppOptions = V.fromList . map T.pack $ Slow.cppOptions bi
    , ccOptions = V.fromList . map T.pack $ Slow.ccOptions bi
    , ldOptions = V.fromList . map T.pack $ Slow.ldOptions bi
    , pkgconfigDepends = V.fromList . map depFromSlow $ Slow.pkgconfigDepends bi
    , frameworks = V.fromList . map T.pack $ Slow.frameworks bi
    , cSources = V.fromList . map filePathFromSlow $ Slow.cSources bi
    , hsSourceDirs = V.fromList . map filePathFromSlow $ Slow.hsSourceDirs bi
    , otherModules = V.fromList . map moduleNameFromSlow $ Slow.otherModules bi
    , defaultLanguage = Slow.defaultLanguage bi
    , otherLanguages = V.fromList $ Slow.otherLanguages  bi
    , defaultExtensions = V.fromList $ Slow.defaultExtensions bi
    , otherExtensions = V.fromList $ Slow.otherExtensions bi
    , oldExtensions = V.fromList $ Slow.oldExtensions bi
    , extraLibs = V.fromList . map T.pack $ Slow.extraLibs bi
    , extraLibDirs = V.fromList . map T.pack $ Slow.extraLibDirs bi
    , includeDirs = V.fromList . map filePathFromSlow $ Slow.includeDirs bi
    , includes = V.fromList . map filePathFromSlow $ Slow.includes bi
    , installIncludes = V.fromList . map filePathFromSlow $ Slow.installIncludes bi
    , options = V.fromList 
              . map (second $ V.fromList . map T.pack )
              $ Slow.options bi
    , ghcProfOptions = V.fromList . map T.pack $ Slow.ghcProfOptions bi
    , ghcSharedOptions = V.fromList. map T.pack  $ Slow.ghcSharedOptions bi
    , customFieldsBI = V.fromList 
                     . map (T.pack *** T.pack)
                     $ Slow.customFieldsBI bi
    , targetBuildDepends = V.fromList . map depFromSlow $ Slow.targetBuildDepends bi
    }

{-# WARNING toSlow "STOP! This has not yet been implemented." #-}

-- | Necessarily lossy, as our version omits some fields.
toSlow :: PackageDescription -> Slow.PackageDescription
toSlow = error "toSlow: not yet implemenetd"



-- transplants from Slow.PackageDescription
-- TODO: convert these better

flattenPackageDescription :: GenericPackageDescription -> PackageDescription
flattenPackageDescription (GenericPackageDescription pkg _ mlib0 exes0 tests0 bms0) =
    pkg { library = mlib
        , executables = V.fromList $ reverse exes
        , testSuites = V.fromList $ reverse tests
        , benchmarks = V.fromList $ reverse bms
        , buildDepends = V.fromList $ ldeps ++ reverse edeps ++ reverse tdeps ++ reverse bdeps
        }
  where
    (mlib, ldeps) = case mlib0 of
        Just lib -> let (l,ds) = ignoreConditions lib in
                    (Just (libFillInDefaults l), ds)
        Nothing -> (Nothing, [])
    (exes, edeps) = V.foldr flattenExe ([],[]) exes0
    (tests, tdeps) = V.foldr flattenTst ([],[]) tests0
    (bms, bdeps) = V.foldr flattenBm ([],[]) bms0
    flattenExe (n, t) (es, ds) =
        let (e, ds') = ignoreConditions t in
        ( (exeFillInDefaults $ e { exeName = n }) : es, ds' ++ ds )
    flattenTst (n, t) (es, ds) =
        let (e, ds') = ignoreConditions t in
        ( (testFillInDefaults $ e { testName = n }) : es, ds' ++ ds )
    flattenBm (n, t) (es, ds) =
        let (e, ds') = ignoreConditions t in
        ( (benchFillInDefaults $ e { benchmarkName = n }) : es, ds' ++ ds )

libFillInDefaults :: Library -> Library
libFillInDefaults lib@(Library { libBuildInfo = bi }) =
    lib { libBuildInfo = biFillInDefaults bi }

exeFillInDefaults :: Executable -> Executable
exeFillInDefaults exe@(Executable { buildInfo = bi }) =
    exe { buildInfo = biFillInDefaults bi }

testFillInDefaults :: TestSuite -> TestSuite
testFillInDefaults tst@(TestSuite { testBuildInfo = bi }) =
    tst { testBuildInfo = biFillInDefaults bi }

benchFillInDefaults :: Benchmark -> Benchmark
benchFillInDefaults bm@(Benchmark { benchmarkBuildInfo = bi }) =
    bm { benchmarkBuildInfo = biFillInDefaults bi }

biFillInDefaults :: BuildInfo -> BuildInfo
biFillInDefaults bi =
    if V.null (hsSourceDirs bi)
    then bi { hsSourceDirs = V.singleton $ currentDir }
    else bi

ignoreConditions :: (Monoid a, Monoid c) => CondTree v c a -> (a, c)
ignoreConditions (CondNode a c ifs) = (a, c) `mappend` mconcat (concatMap f ifs)
  where f (_, t, me) = ignoreConditions t
                       : maybeToList (fmap ignoreConditions me)






