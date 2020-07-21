{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Based on https://github.com/nwf/xconfig/blob/master/xmonad/lib/XMonad/Actions/Xmobars.hs
module Blueberry.Statuslines where

import XMonad
import qualified XMonad.Hooks.DynamicLog as HDL
import qualified XMonad.Layout.IndependentScreens as LIS
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as UE
import qualified XMonad.Util.Run as UR
import qualified XMonad.Util.WindowProperties as UW

import Control.Exception (SomeException, handle)
import Control.Monad (foldM, when)
import qualified Data.IntMap as IM
import Data.List (find, stripPrefix)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (All(..))
import System.IO (Handle, hClose, hPutStrLn)
import System.Posix.Signals (signalProcess, keyboardSignal)
import System.Posix.Types (ProcessID)

data XMobars = XMobars
  { xmPidScreen :: M.Map ProcessID ScreenId
  , xmScreenState :: IM.IntMap (Handle, ProcessID)
  , xmScreenIntent :: IM.IntMap (Maybe String)
  , xmWinScreen :: M.Map Window ScreenId
  , xmScreenWin :: IM.IntMap Window
  } deriving (Typeable)

instance ExtensionClass XMobars where
  initialValue = XMobars M.empty IM.empty IM.empty M.empty IM.empty
  extensionType = PersistentExtension

instance Show XMobars where
  show x = "XMobars (" ++ (show $ xmScreenIntent x) ++ ")"

instance Read XMobars where
  readsPrec _ r = [ (initialValue {xmScreenIntent = xmsi}, s) |
                    ("XMobars", r') <- lex r,
                    (xmsi, s) <- readsPrec 11 r'
                  ]

killxmobar :: Bool -> XMobars -> ScreenId -> IO XMobars
killxmobar dokill xmbs (S ix) = do
  case IM.lookup ix (xmScreenState xmbs) of
    Nothing -> return xmbs
    Just (h, pid) -> do
      when dokill $ killpid pid
      ignoreExn (hClose h)
      return $ xmbs
        { xmPidScreen = M.delete pid (xmPidScreen xmbs)
        , xmScreenState = IM.delete ix (xmScreenState xmbs)
        , xmScreenWin = IM.delete ix (xmScreenWin xmbs)
        , xmScreenIntent = IM.delete ix (xmScreenIntent xmbs)
        , xmWinScreen = maybe (xmWinScreen xmbs)
                              (\w -> M.delete w (xmWinScreen xmbs))
                              (IM.lookup ix (xmScreenWin xmbs))
        }

updatexmobar :: XMobars -> ScreenId -> IO XMobars
updatexmobar xmbs (S ix) = do
  let mcfg = IM.lookup ix (xmScreenIntent xmbs)
  case mcfg of 
    Just (Just cfg) -> case IM.lookup ix (xmScreenState xmbs) of
      Nothing -> do
        r@(_, pid) <- UR.spawnPipePid $
                      "exec xmobar -x " ++ (show ix)
                      ++ " " ++ cfg
        let nps = M.insert pid (S ix) (xmPidScreen xmbs)
        let ns = IM.insert ix r (xmScreenState xmbs)
        return $ xmbs { xmPidScreen = nps, xmScreenState = ns }
      Just _ -> return xmbs
    _ -> killxmobar True xmbs (S ix)

updatexmobars_ :: XMobars -> X XMobars
updatexmobars_ xmbs = do
  screencount <- LIS.countScreens
  let (_, mkill, okill) = IM.splitLookup screencount (xmScreenState xmbs)
  let kills = maybe (id) (:) mkill (IM.elems okill)

  mapM_ (\(h,p) -> io $ killpid p >> ignoreExn (hClose h)) kills
  let sk = map snd kills
  let nps = foldl (flip M.delete) (xmPidScreen xmbs) sk
  let ws = map (flip IM.lookup (xmScreenWin xmbs) . fromIntegral) sk
  let nsw = foldl (\a b -> IM.delete (fromIntegral b) a) (xmScreenWin xmbs) sk
  let nws = foldl (\a b -> case b of
                            Nothing -> a
                            Just x' -> M.delete x' a) (xmWinScreen xmbs) ws
  foldM (\a b -> io $ updatexmobar a (S b))
        (xmbs { xmPidScreen = nps
              , xmWinScreen = nws
              , xmScreenWin = nsw
              })
        (IM.keys $ fst . IM.split screencount $ xmScreenIntent xmbs)

updatexmobars :: X ()
updatexmobars = UE.put =<< updatexmobars_ =<< UE.get

ensureanxmobar_ :: ScreenId -> String -> XMobars -> XMobars
ensureanxmobar_ (S s) c xmbs =
  case IM.lookup s (xmScreenIntent xmbs) of
    Just (Just _) -> xmbs
    _ -> xmbs { xmScreenIntent = IM.insert s (Just c)
                                    (xmScreenIntent xmbs) }

ensureanxmobar :: ScreenId -> String -> X ()
ensureanxmobar s c = UE.put =<< return . ensureanxmobar_ s c =<< UE.get

toggleanxmobar :: ScreenId -> X ()
toggleanxmobar (S scr) = do
  xmbs <- UE.get
  let nmcfg = case IM.lookup scr (xmScreenIntent xmbs) of
                Just (Just _) -> Nothing
                _ -> Just "$HOME/.config/xmobar/xmobarrc0"
  let xmbs' = xmbs {xmScreenIntent = IM.insert scr nmcfg (xmScreenIntent xmbs)}
  UE.put =<< (liftIO $ updatexmobar xmbs' (S scr))

togglemyxmobar :: X ()
togglemyxmobar = do
  cws <- gets windowset
  toggleanxmobar (S.screen $ S.current $ cws)

killxmobars :: X ()
killxmobars = do
  xmbs <- UE.get
  UE.put =<< foldM (\xs' s -> io $ killxmobar True xs' (S s)) xmbs
                   (IM.keys $ xmScreenWin xmbs)


