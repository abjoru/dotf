module Blueberry.Utils (
  sanitizeName, listGames
) where

import Control.Applicative
import Control.Monad

import System.Directory
import System.FilePath

import Data.Tree
import Data.List (find)
import Data.Maybe (catMaybes)

import XMonad
import qualified XMonad.Actions.TreeSelect as TS

-- Sanitize a name such that any characters like '-_"' have 
-- either been removed or replaced with space.
sanitizeName :: String -> String
sanitizeName ('-':xs)   = ' ' : sanitizeName xs
sanitizeName ('_':xs)   = ' ' : sanitizeName xs
sanitizeName ('"':xs)   = sanitizeName xs
sanitizeName (x:xs)     = x : sanitizeName xs
sanitizeName ""         = ""

-- Attempt to extract game launcher script from contents
maybeLauncher :: [FilePath] -> Maybe FilePath
maybeLauncher = find (\a -> ".sh" == takeExtension a)

-- Combine game dir with launcher file if any
mkEntry :: FilePath -> Maybe FilePath -> Maybe (String, FilePath)
mkEntry a (Just b)  = Just (sanitizeName $ takeBaseName a, b)
mkEntry a Nothing = Nothing

-- Convert game folder into a pair (name, launcher)
processGame :: FilePath -> IO (Maybe (String, FilePath))
processGame p = do
  contents <- listAbsDir p
  let a = maybeLauncher contents
      b = mkEntry p a
  return b

-- List games in the given directory
listGames :: FilePath -> IO [(String, FilePath)]
listGames p = do
  files <- listAbsDir p
  maybeGames <- sequence $ map processGame files
  return $ catMaybes maybeGames

-- List directory contents as absolute paths
listAbsDir :: FilePath -> IO [FilePath]
listAbsDir d = listDirectory d >>= mapM (canonicalizePath . (d </>))
