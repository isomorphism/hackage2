-- (C) Copyright by Bas van Dijk, v.dijk.bas@gmail.com, 2008
-- Inspiration (read: copied, renamed and simplified) from:
-- http://code.haskell.org/haddock/src/Haddock/ModuleTree.hs

module Distribution.Server.Packages.ModuleForest ( ModuleForest, ModuleTree(..), moduleForest ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Compact.ModuleName ( ModuleName, components )

--------------------------------------------------------------------------------

type ModuleForest = [ModuleTree]

data ModuleTree = Node Text         -- Part of module name
                       Bool         -- Is this an existing module?
                       ModuleForest -- Sub modules
    deriving (Show, Eq)

--------------------------------------------------------------------------------

moduleForest :: Vector ModuleName -> ModuleForest
moduleForest = V.foldr (addToForest . V.toList . components) []

addToForest :: [Text] -> ModuleForest -> ModuleForest
addToForest [] ts = ts
addToForest ss [] = mkSubTree ss
addToForest s1ss@(s1:ss) (t@(Node s2 isModule subs) : ts)
  | s1 >  s2  = t : addToForest s1ss ts
  | s1 == s2  = Node s2 (isModule || null ss) (addToForest ss subs) : ts
  | otherwise = mkSubTree s1ss ++ t : ts

mkSubTree :: [Text] -> ModuleForest
mkSubTree []     = []
mkSubTree (s:ss) = [Node s (null ss) (mkSubTree ss)]

--------------------------------------------------------------------------------

