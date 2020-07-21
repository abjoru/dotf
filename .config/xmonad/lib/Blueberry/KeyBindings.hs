module Blueberry.KeyBindings where

import Blueberry.Variables
import Blueberry.Prompts
import Blueberry.GridSelect
import Blueberry.TreeMenu

import Data.Maybe (isJust)
import System.Exit (exitSuccess)

-- Base
import XMonad
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), prevScreen, nextScreen)
import XMonad.Actions.Promote
import XMonad.Actions.GridSelect
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

-- Hooks
import XMonad.Hooks.ManageDocks (ToggleStruts(..))

-- Layouts and modifiers
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NOBORDERS))
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger (WindowArrangerMsg(..))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)

-- Utilities
import XMonad.Util.Run (safeSpawn)

------------------------------------------------------------------------
-- KEYBINDINGS (using XMonad.Util.EZConfig)
------------------------------------------------------------------------
myKeys :: [([Char], X ())]
myKeys =
  -- Xmonad
  [ ("M-C-r", spawn "xmonad --recompile")               -- Recompiles xmonad
  , ("M-S-r", spawn "xmonad --restart")                 -- Restarts xmonad
  , ("M-S-q", io exitSuccess)                           -- Quits xmonad

  -- Prompts
  , ("M-S-<Return>", shellPrompt myXPConfig)            -- Shell prompt

  -- Windows
  , ("M-S-c", kill1)                                    -- Kill the currently focused client
  , ("M-S-a", killAll)                                  -- Kill all the windows on current workspace

  -- Floating windows
  , ("M-<Delete>", withFocused $ windows . W.sink)      -- Push floating window back to tile
  , ("M-S-<Delete>", sinkAll)                           -- Push all floating windows back to tile

  -- Grid Select
  , ("C-g a", spawnSelected' myAppGrid)                 -- grid select favourite apps
  , ("C-g c", spawnSelected' myConfigGrid)              -- grid select useful config files
  , ("C-g t", goToSelected $ myGridConfig myColorizer)  -- goto selected
  , ("C-g b", bringSelected $ myGridConfig myColorizer) -- bring selected

  -- TreeSelect
  , ("C-t t", treeSelectAction tsDefaultConfig)

  -- Window navigation
  , ("M-m", windows W.focusMaster)                      -- Move focus to the master window
  , ("M-j", windows W.focusDown)                        -- Move focus to the next window
  , ("M-k", windows W.focusUp)                          -- Move focus to the prev window
  , ("M-S-j", windows W.swapDown)                       -- Swap the focused window with the next window
  , ("M-S-k", windows W.swapUp)                         -- Swap the focused window with the prev window
  , ("M-<Backspace>", promote)                          -- Moves focused window to master, all others maintain order
  , ("M1-S-<Tab>", rotSlavesDown)                       -- Rotate all windows except master and keep focus in place
  , ("M1-C-<Tab>", rotAllDown)                          -- Rotate all the windows in the current stack
  , ("M-C-s", killAllOtherCopies)

  , ("M-C-M1-<Up>", sendMessage Arrange)
  , ("M-C-M1-<Down>", sendMessage DeArrange)
  , ("M-<Up>", sendMessage (MoveUp 10))                 -- Move focused window to up
  , ("M-<Down>", sendMessage (MoveDown 10))             -- Move focused window to down
  , ("M-<Right>", sendMessage (MoveRight 10))           -- Move focused window to right
  , ("M-<Left>", sendMessage (MoveLeft 10))             -- Move focused window to left
  , ("M-S-<Up>", sendMessage (IncreaseUp 10))           -- Increase size of focused window up
  , ("M-S-<Down>", sendMessage (IncreaseDown 10))       -- Increase size of focused window down
  , ("M-S-<Right>", sendMessage (IncreaseRight 10))     -- Increase size of focused window right
  , ("M-S-<Left>", sendMessage (IncreaseLeft 10))       -- Increase size of focused window left
  , ("M-C-<Up>", sendMessage (DecreaseUp 10))           -- Decrease size of focused window up
  , ("M-C-<Down>", sendMessage (DecreaseDown 10))       -- Decrease size of focused window down
  , ("M-C-<Right>", sendMessage (DecreaseRight 10))     -- Decrease size of focused window right
  , ("M-C-<Left>", sendMessage (DecreaseLeft 10))       -- Decrease size of focused window left

  -- Layouts
  , ("M-<Tab>", sendMessage NextLayout)                                 -- Switch to next layout
  , ("M-S-<Space>", sendMessage ToggleStruts)                           -- Toggles struts
  , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)                        -- Toggles noborder
  , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))     -- Increase number of clients in the master pane
  , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))    -- Decrease number of clients in the master pane
  , ("M-S-<KP_Multiply", increaseLimit)                 -- Increase number of windows that can be shown
  , ("M-S-<KP_Divide", decreaseLimit)                   -- Decrease number of windows that can be shown

  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)

  -- Workspaces
  , ("M-.", nextScreen)                                                 -- Switch focus to next monitor
  , ("M-,", prevScreen)                                                 -- Switch focus to prev monitor
  , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)         -- Shifts focused window to next workspace
  , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)    -- Shifts focused window to prev workspace

  -- Scratchpads
  --, ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
  --, ("M-C-c", namedScratchpadAction myScratchPads "cmus")

  -- Open My Preffered Terminal.
  , ("M-<Return>", spawn myTerminal)
  , ("M1-<Return>", spawn myTerminal)

  -- My Applications (Super+Alt+Key)
  , ("M-M1-a", spawn (myTerminal ++ " -e ncpamixer"))
  , ("M-M1-b", spawn ("surf www.youtube.com"))
  , ("M-M1-c", spawn (myTerminal ++ " -e cmus"))
  , ("M-M1-e", spawn (myTerminal ++ " -e neomutt"))
  , ("M-M1-f", spawn (myTerminal ++ " -e sh ./.config/vifm/scripts/vifmrun"))
  , ("M-M1-i", spawn (myTerminal ++ " -e irssi"))
  , ("M-M1-j", spawn (myTerminal ++ " -e joplin"))
  , ("M-M1-l", spawn (myTerminal ++ " -e lynx -cfg=~/.lynx/lynx.cfg -lss=~/.lynx/lynx.lss http://google.com"))
  , ("M-M1-m", spawn (myTerminal ++ " -e toot curses"))
  , ("M-M1-n", spawn (myTerminal ++ " -e newsboat"))
  , ("M-M1-p", spawn (myTerminal ++ " -e pianobar"))
  , ("M-M1-r", spawn (myTerminal ++ " -e rtv"))
  , ("M-M1-w", spawn (myTerminal ++ " -e wopr report.xml"))
  , ("M-M1-y", spawn (myTerminal ++ " -e youtube-viewer"))

  -- Multimedia Keys
  , ("<XF86AudioPlay>", spawn "cmus toggle")
  , ("<XF86AudioPrev>", spawn "cmus prev")
  , ("<XF86AudioNext>", spawn "cmus next")
  --, ("<XF86AudioMute>", spawn "amixer set Master toggle") -- Bug prevents it from toggling correctly in 12.04
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
  , ("<XF86HomePage>", spawn "firefox")
  , ("<XF86Search>", safeSpawn "firefox" ["https://www.google.com/"])
  --, ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
  , ("<XF86Mail>", spawn (myTerminal ++ " -e neomutt"))
  , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
  , ("<XF86Eject>", spawn "toggleeject")
  , ("<Print>", spawn "scrotd 0")
  ] 
  -- Appending search engines to keybinding list
  ++ [("M-s " ++ k, S.promptSearch myXPConfig f) | (k,f) <- searchList ]
  ++ [("M-p " ++ k, f myXPConfig) | (k,f) <- promptList ]
  -- Appending named scratchpads to keybinding list
    where nonNSP                = WSIs (return (\ws -> W.tag ws /= "nsp"))
          nonEmptyNonNSP        = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))
