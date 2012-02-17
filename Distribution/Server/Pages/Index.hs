-- Generate an HTML page listing all available packages

module Distribution.Server.Pages.Index (packageIndex) where

import Control.Applicative
import Distribution.Server.Pages.Template	( hackagePage )

import Distribution.FastPackageDescription
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo(..))
import Distribution.Simple.Utils (comparing, equating)
import Distribution.Compact.ModuleName (toFilePath)

import Text.XHtml.Strict hiding ( p, name )
import qualified Text.XHtml.Strict as XHtml ( name )

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower, toUpper, isSpace)
import Data.List (intersperse, sortBy, groupBy, nub, maximumBy)


packageIndex :: PackageIndex.PackageIndex PkgInfo -> Html
packageIndex = formatPkgGroups
                 . map (flattenPackageDescription
                      . pkgDesc
                      . maximumBy (comparing packageVersion))
                 . PackageIndex.allPackagesByName

data Category = Category Text | NoCategory
	deriving (Eq, Ord, Show)

-- Packages, grouped by category and ordered by name with each category.
formatPkgGroups :: [PackageDescription] -> Html
formatPkgGroups pkgs = hackagePage "packages by category" docBody
  where docBody =
		(thediv ! [theclass "floatright"] << searchBox) :
		(h2 << "Packages by category") :
		-- table of contents
		paragraph ! [theclass "toc"] <<
			(bold << "Categories:" : toHtml " " :
			 intersperse (toHtml ", ") (map catLink cat_pkgs) ++
			 [toHtml "."]) :
		-- packages grouped by category
		[formatCategory cat +++
			formatPkgList (sortBy (comparing sortKey) sub_pkgs) |
			(cat, sub_pkgs) <- cat_pkgs]
	searchBox =
		[form ! [method "get", action "http://www.google.co.uk/search"] <<
			[input ! [thetype "hidden", XHtml.name "hl", value "en"],
			 input ! [thetype "hidden", XHtml.name "as_sitesearch", value "hackage.haskell.org/packages"],
			 input ! [thetype "text", size "20", XHtml.name "as_q", value ""],
			 input ! [thetype "submit", value "Search package pages"]
			]]
	catLink (cat, sub_pkgs) =
		(anchor ! [href ("#" ++ catLabel catName)] << catName) +++
		spaceHtml +++
		toHtml ("(" ++ show (length sub_pkgs) ++ ")")
	  where catName = T.unpack $ categoryName cat
	cat_pkgs = groupOnFstBy normalizeCategory $ [(capitalize cat, pkg) |
			pkg <- pkgs, cat <- categories pkg]
	sortKey pkg = T.toLower $ unPackageName $ pkgName $ package pkg
	formatCategory cat =
		h3 ! [theclass "category"] <<
			anchor ! [XHtml.name (catLabel catName)] << catName
	  where catName = T.unpack $ categoryName cat
	catLabel cat = "cat:" ++ cat
	categoryName (Category cat) = cat
	categoryName NoCategory = T.pack "Unclassified"
	capitalize (Category s) =
		Category (T.unwords . map (\w -> T.toUpper (T.take 1 w) `T.append`  T.drop 1 w) . T.words $ s)
	capitalize NoCategory = NoCategory

formatPkgList :: [PackageDescription] -> Html
formatPkgList pkgs = ulist ! [theclass "packages"] << map formatPkg pkgs

formatPkg :: PackageDescription -> Html
formatPkg pkg = li << (pkgLink : toHtml (" " ++ ptype) : defn)
  where pname = pkgName (package pkg)
	pkgLink = anchor ! [href (packageNameURL pname)] << T.unpack (unPackageName pname)
	defn
	  | T.null (synopsis pkg) = []
	  | otherwise = [toHtml (": " ++ T.unpack (trim $ synopsis pkg))]
	ptype
	  | V.null (executables pkg) = "library"
	  | hasLibs pkg = "library and " ++ programs
	  | otherwise = programs
	  where programs
		  | V.length (executables pkg) > 1 = "programs"
		  | otherwise = "program"
	trim s
	  | T.length s < 90 = s
	  | otherwise = (T.dropWhileEnd (/= ',') . T.take 76 $ s) `T.append` T.pack " ..."

categories :: PackageDescription -> [Category]
categories pkg
  | not (T.null cats) && (cats `notElem` blacklist) = split cats
  | not (null top_level_nodes) && length top_level_nodes < 3 &&
	all (`elem` allocatedTopLevelNodes) top_level_nodes =
	map (Category . T.pack) top_level_nodes
  | otherwise = [NoCategory]
  where cats = trim (category pkg)
	-- trim will not be necessary with future releases of cabal
	trim = T.dropWhileEnd isSpace
	split cs = let (front, back) = T.break (== ',') cs
               in if T.null back 
                  then [Category front]
                  else Category front : split (T.dropWhile isSpace $ T.drop 1 back)
	-- if no category specified, use top-level of module hierarchy
	top_level_nodes =
		maybe [] (nub . map (takeWhile (/= '.') . toFilePath) . V.toList . exposedModules)
		(library pkg)

-- categories we ignore
blacklist :: [Text]
blacklist = T.pack <$> [ "Application", "Foreign binding", "Tool", "Type"
                       , "Various", "Unclassified"]

groupOnFstBy :: (Ord a, Ord c) => (a -> c) -> [(a, b)] -> [(a, [b])]
groupOnFstBy f xys = [(x, y : map snd xys') |
	(x, y) : xys' <- groupBy (equating (f . fst)) (sortBy (comparing sortKey) xys)]
  where sortKey (x, _) = (f x, x)

normalizeCategory :: Category -> Category
normalizeCategory (Category n) = Category (T.toLower n)
normalizeCategory NoCategory = NoCategory

allocatedTopLevelNodes :: [String]
allocatedTopLevelNodes = [
	"Algebra", "Codec", "Control", "Data", "Database", "Debug",
	"Distribution", "DotNet", "Foreign", "Graphics", "Language",
	"Network", "Numeric", "Prelude", "Sound", "System", "Test", "Text"]

packageNameURL :: PackageName -> URL
packageNameURL pkg = "/package/" ++ T.unpack (unPackageName pkg)

unPackageName :: PackageName -> Text
unPackageName (PackageName name) = name
