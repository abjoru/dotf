{-# LANGUAGE OverloadedStrings #-}
module Blueberry.Monitors (spawnBerrybars, xmobarOutput) where

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

-- Get X screen counts (i.e. num monitors)
countScreens :: IO Int
countScreens = do
  screens <- do
    dpy   <- openDisplay ""
    rects <- getScreenInfo dpy
    closeDisplay dpy
    return rects
  pure $ length screens

-- Setup dynamicLogWithPP outputs
xmobarOutput :: [Handle] -> String -> IO ()
xmobarOutput ha x = mapM_ (\h -> hPutStrLn h x) ha

-- Create blueberry-bars for each screen
spawnBerrybars :: IO [Handle]
spawnBerrybars = countScreens >>= spawnBerrybars'

-- Tailor to screen setup...
spawnBerrybars' :: Int -> IO [Handle]
spawnBerrybars' 0 = fmap (\x -> [x]) (spawnPipe "/home/abjoru/.local/bin/blueberry-mobar single")
spawnBerrybars' 1 = fmap (\x -> [x]) (spawnPipe "/home/abjoru/.local/bin/blueberry-mobar single")
spawnBerrybars' 2 = do
  let arg = [(0, "primary"), (1, "secondary")]
  mapM (\(i, m) -> spawnPipe $ blueberryMobar i m) arg
spawnBerrybars' 3 = do
  let arg = [(0, "primary"), (1, "secondary"), (2, "other")]
  mapM (\(i, m) -> spawnPipe $ blueberryMobar i m) arg
spawnBerrybars' 4 = do
  let arg = [(0, "primary"), (1, "other"), (2, "other"), (3, "secondary")]
  mapM (\(i, m) -> spawnPipe $ blueberryMobar i m) arg
spawnBerrybars' n = do
  let idx = take n [0..]
  let mon = ["primary", "secondary"] ++ (take (n - 2) (repeat "other"))
  let arg = zip idx mon
  mapM (\(i, m) -> spawnPipe $ blueberryMobar i m) arg

-- Executable string for a blueberry-mobar
blueberryMobar :: Int -> String -> String
blueberryMobar i m = "/home/abjoru/.local/bin/blueberry-mobar -x " ++ (show i) ++ " " ++ m
