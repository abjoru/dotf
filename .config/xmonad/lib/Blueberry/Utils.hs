module Blueberry.Utils (
  sanitizeName, getGames
) where

import Control.Applicative
import Control.Monad

import System.Directory
import System.FilePath

-- Sanitize a name such that any characters like '-_"' have 
-- either been removed or replaced with space.
sanitizeName :: (Show a) => a -> String
sanitizeName ('-':xs)   = ' ' : sanitizeName xs
sanitizeName ('_':xs)   = ' ' : sanitizeName xs
sanitizeName ('"':xs)   = sanitizeName xs
sanitizeName ""         = ""

-- Filter for *.sh files
shellFileFilter :: FilePath -> Bool
shellFileFilter f = takeExtension f == ".sh"

getGames :: IO [(String, FilePath, String)]
getGames = mapM processGame <$> listDirectory "/home/abjoru/Games"

processGame :: FilePath -> IO [Maybe (String, FilePath, String)]
processGame p = mapN (\a -> (p, a)) <$> filter shellFileFilter <$> listDirectory p
