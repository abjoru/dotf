-- The xmonad configuration

------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------

import Blueberry.Palette
import Blueberry.Variables
import Blueberry.Prompts
import Blueberry.Layouts
import Blueberry.GridSelect
import Blueberry.KeyBindings

  -- Base
import XMonad

  -- System
import System.IO (hPutStrLn)

  -- Data
import Data.Monoid

  -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ServerMode
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doCenterFloat, doFullFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

  -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "$HOME/.config/xmonad/scripts/autostart.sh"
  setWMName "LG3D"

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
-- My workspaces are clickable meaning that the mouse can be used to 
-- switch workspaces. This requires xdotool.

xmobarEscape :: [Char] -> [Char]
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = ["dev", "www", "sys", "doc", "vbox", "chat", "mus", "vid", "gfx"]

------------------------------------------------------------------------
-- MANAGEHOOKS
------------------------------------------------------------------------
-- Sets some rules for certain programs. Examples include forcing certain
-- programs to always float, or to always appear on a certain workspace.
-- Forcing programs to a certain workspace with a doShift requires xdotool
-- if you are using clickable workspaces. You need the className or title
-- of the program. Use xprop to get this info.

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll . concat $
  [ [isDialog --> doCenterFloat]
  , [className =? c --> doCenterFloat | c <- myCFloats]
  , [title =? t --> doFloat | t <- myTFloats]
  , [resource =? r --> doFloat | r <- myRFloats]
  , [resource =? i --> doIgnore | i <- myIFloats]
  , [className =? c --> doShift (myWorkspaces !! 0) | c <- myC0Shifts]
  , [className =? c --> doShift (myWorkspaces !! 1) | c <- myC1Shifts]
  , [title =? t --> doShift (myWorkspaces !! 0) | t <- myT0Shifts]
  , [title =? t --> doShift (myWorkspaces !! 1) | t <- myT1Shifts]
  ]
  where
    myCFloats  = ["Arandr", "Gimp", "Galculator", "feh", "mpv", "streamdeck"]
    myTFloats  = ["Downloads", "Save As...", "Oracle VM VirtualBox Manager"]
    myRFloats  = []
    myIFloats  = ["desktop_window"]
    myC0Shifts = ["Oracle VM VirtualBox Manager"]
    myC1Shifts = []
    myT0Shifts = []
    myT1Shifts = ["firefox"]

------------------------------------------------------------------------
-- LOGHOOK
------------------------------------------------------------------------
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.9

------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/abjoru/.config/xmobar/xmobarrc0"
  xmproc1 <- spawnPipe "xmobar -x 1 /home/abjoru/.config/xmobar/xmobarrc1"
  xmproc2 <- spawnPipe "xmobar -x 2 /home/abjoru/.config/xmobar/xmobarrc2"
  xmproc3 <- spawnPipe "xmobar -x 3 /home/abjoru/.config/xmobar/xmobarrc3"
  xmonad $ ewmh $ def
    { manageHook         = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
    , handleEventHook 	 = serverModeEventHookCmd
			   <+> serverModeEventHook
			   <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
			   <+> docksEventHook
    , modMask 		 = myModMask
    , terminal 		 = myTerminal
    , startupHook 	 = myStartupHook
    , layoutHook         = myLayoutHook
    , workspaces 	 = myWorkspaces
    , borderWidth 	 = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , logHook            = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                            { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x >> hPutStrLn xmproc3 x
                            , ppCurrent = xmobarColor pGreen0 "" . wrap "[" "]" -- Current workspace in xmobar
                            , ppVisible = xmobarColor pGreen1 ""                -- Visible but not current workspace
                            , ppHidden = xmobarColor pBlue0 "" . wrap "*" ""   -- Hidden workspaces in xmobar
                            , ppHiddenNoWindows = xmobarColor pYellow0 ""        -- Hidden workspaces (no windows)
                            , ppTitle = xmobarColor pGray0 "" . shorten 60     -- Title of active window in xmobar
                            , ppSep =  " | "                     -- Separators in xmobar
                            , ppUrgent = xmobarColor pOrange0 "" . wrap "!" "!"  -- Urgent workspace
                            , ppExtras  = [windowCount]                           -- # of windows current workspace
                            , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                            }
    } `additionalKeysP` myKeys
