{-# LANGUAGE OverloadedStrings #-}
module Blueberry.Monitors (spawnXmobars, xmobarOutput) where

import Blueberry.Variables (configDirDotf, configDirXmobar)

import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig))
import System.FilePath

import Data.Ini (lookupValue, readIniFile)
import qualified Data.Text as T

import XMonad.Util.Run (spawnPipe, hPutStrLn)

import GHC.IO.Handle

import Graphics.X11.Xlib as X11
import Graphics.X11.Xinerama as X11

-----------------------------
-- Blueberry Monitor Utils --
-----------------------------
-- Provide means of dynamic configuration of monitors based 
-- on DotF global config. As of now, this pertains to having 
-- different XMobar configs pr screen.

-- Create xmobar executable command string
xmobarConfig :: Int -> FilePath -> String
xmobarConfig i fp = "xmobar -x " ++ (show i) ++ " " ++ (show fp)

-- Get X screen counts (i.e. num monitors)
xmobarScreenCount :: IO Int
xmobarScreenCount = do
  screens <- do
    dpy   <- openDisplay ""
    rects <- getScreenInfo dpy
    closeDisplay dpy
    return rects
  pure $ length screens

-- Load xmobarrc config files from global config
loadXmobarConfigs :: IO [T.Text]
loadXmobarConfigs = do
  file     <- fmap (\x -> x </> "dotf.cfg") configDirDotf
  maybeIni <- readIniFile file
  let res = maybeIni >>= lookupValue "XMonad" "xmobarrc"
  case res of Left s  -> pure []
              Right s -> pure $ map T.strip $ T.splitOn "," s

-- Get xmobar file paths
xmobarConfigs :: IO [(Int, FilePath)]
xmobarConfigs = do
  cnt <- xmobarScreenCount
  lst <- loadXmobarConfigs
  dir <- configDirXmobar
  let fs = map (\x -> dir </> T.unpack x) lst
  pure $ zip [0..] $ take cnt fs

-- Spawn xmobar instances for all configs
spawnXmobars :: IO [Handle]
spawnXmobars = do
  cfgs <- xmobarConfigs
  mapM (\(y, x) -> spawnPipe $ xmobarConfig y x) cfgs

-- Setup dynamicLogWithPP outputs
xmobarOutput :: [Handle] -> String -> IO ()
xmobarOutput ha x = mapM_ (\h -> hPutStrLn h x) ha
