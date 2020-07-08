-- The xmonad configuration

------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------
  -- Base
import XMonad
--import XMonad.Config.Desktop
--import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

  -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

  -- Data
import Data.Char (isSpace)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
--import Data.List
--import qualified Data.Tuple.Extra as TE
import qualified Data.Map as M

  -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, PP(..))
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docks, docksEventHook, ToggleStruts(..))
import XMonad.Hooks.ServerMode
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doFullFloat, doCenterFloat)
import XMonad.Hooks.SetWMName

  -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
--import XMonad.Layout.OneBig
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
--import XMonad.Layout.ZoomRow (zoomRow, zoomReset, ZoomMessage(ZoomFullToggle))

  -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
--import XMonad.Layout.Reflect (REFLECTX(..), REFLECTY(..))
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

  -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

  -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import XMonad.Util.SpawnOnce

  -- Polybar (req)
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

------------------------------------------------------------------------
-- GRUVBOX PALETTE (DARK)
------------------------------------------------------------------------

gruvBG0 :: [Char]
gruvBG0 = "#282828"

gruvBG1 :: [Char]
gruvBG1 = "#3c3836"

gruvBG2 :: [Char]
gruvBG2 = "#504945"

gruvBG3 :: [Char]
gruvBG3 = "#665c54"

gruvBG4 :: [Char]
gruvBG4 = "#7c6f64"

gruvFG0 :: [Char]
gruvFG0 = "#ebdbb2"

gruvFG1 :: [Char]
gruvFG1 = "#ebdbb2"

gruvFG2 :: [Char]
gruvFG2 = "#d5c4a1"

gruvFG3 :: [Char]
gruvFG3 = "#bdae93"

gruvFG4 :: [Char]
gruvFG4 = "#a89984"

gruvRed0 :: [Char]
gruvRed0 = "#cc241d"

gruvRed1 :: [Char]
gruvRed1 = "#fb4934"

gruvGreen0 :: [Char]
gruvGreen0 = "#98971a"

gruvGreen1 :: [Char]
gruvGreen1 = "#b8bb26"

gruvYellow0 :: [Char]
gruvYellow0 = "#d79921"

gruvYellow1 :: [Char]
gruvYellow1 = "#fabd2f"

gruvBlue0 :: [Char]
gruvBlue0 = "#458588"

gruvBlue1 :: [Char]
gruvBlue1 = "#83a598"

gruvPurple0 :: [Char]
gruvPurple0 = "#b16286"

gruvPurple1 :: [Char]
gruvPurple1 = "#d3869b"

gruvAqua0 :: [Char]
gruvAqua0 = "#689d6a"

gruvAqua1 :: [Char]
gruvAqua1 = "#8ec07c"

gruvGray0 :: [Char]
gruvGray0 = "#a89984"

gruvGray1 :: [Char]
gruvGray1 = "#928374"

gruvOrange0 :: [Char]
gruvOrange0 = "#d65d0e"

gruvOrange1 :: [Char]
gruvOrange1 = "#fe8019"

------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------
myFont :: [Char]
myFont = "xft:Mononoki Nerd Font:bold:pixelsize=11"

-- sets modkey to super/windows key
myModMask :: KeyMask
myModMask = mod4Mask 

-- sets default terminal
myTerminal :: [Char]
myTerminal = "alacritty" 
--myTerminal = "termonad"

-- sets default browser for tree select
myBrowser :: String
myBrowser = myTerminal ++ " -e lynx "

-- sets default editor for tree select
myEditor :: String
myEditor = myTerminal ++ " -e nvim "

-- sets border width for windows
myBorderWidth :: Dimension
myBorderWidth = 2

-- border color of normal windows
myNormColor :: [Char]
--myNormColor = "#292d3e"
myNormColor = gruvBG0

-- border color of focused windows
myFocusColor :: [Char]
--myFocusColor = "#bbc5ff"
myFocusColor = gruvFG1

-- setting this for use in xprompts
altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawn "$HOME/.config/xmonad/scripts/autostart.sh"
  setWMName "LG3D"

------------------------------------------------------------------------
-- GRID SELECT
------------------------------------------------------------------------
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
  (0x1d,0x20,0x21) -- #1d2021
  (0x28,0x28,0x28) -- #282828
  (0x66,0x5c,0x54) -- #665c54
  (0xa8,0x99,0x84) -- #a89984
  (0xfb,0xf1,0xc7) -- #fbf1c7

-- gridSelect menu layout
myGridConfig :: p -> GSConfig Window
myGridConfig colorizer = (buildDefaultGSConfig myColorizer)
  { gs_cellheight = 40
  , gs_cellwidth = 250
  , gs_cellpadding = 6
  , gs_originFractX = 0.5
  , gs_originFractY = 0.5
  , gs_font = myFont
  }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where conf = def

-- The lists below are actually 3-tuples for use with gridSelect and treeSelect.
-- TreeSelect uses all three values in the 3-tuples but GridSelect only needs first
-- two values in each list
myApplications :: [(String, String, String)]
myApplications = [ ("Firefox", "firefox", "The famous open source web browser")
                 , ("PCManFM", "pcmanfm", "Lightweight graphical file manager")
                 , ("ThinkOrSwim", "thinkorswim", "TD Ameritrade platform")
                 , ("Steam", "steam", "Proprietary gaming platform")
                 , ("QuteBrowser", "qutebrowser", "Simple VIM-like web browser")
                 , ("PrusaSlicer", "slicer", "Prusa 3d printer slicer software")
                 , ("VirtualBox", "virtualbox", "Virtualization software")
                 ]

myConfigs :: [(String, String, String)]
myConfigs = [ ("xmonad", myEditor ++ "/home/abjoru/.config/xmonad/xmonad.hs", "xmonad config")
            -- , ("xmobarrc0", myEditor ++ "/home/abjoru/.config/xmobar/xmobarrc0", "xmobar config for screen 0")
            -- , ("xmobarrc1", myEditor ++ "/home/abjoru/.config/xmobar/xmobarrc1", "xmobar config for screen 1")
            -- , ("xmobarrc2", myEditor ++ "/home/abjoru/.config/xmobar/xmobarrc2", "xmobar config for screen 2")
            -- , ("xmobarrc3", myEditor ++ "/home/abjoru/.config/xmobar/xmobarrc3", "xmobar config for screen 3")
            , ("polybar", myEditor ++ "/home/abjoru/.config/polybar/config", "Polybar config file")
            , ("zshrc", myEditor ++ "/home/abjoru/.config/zsh/config.zsh", "zsh config")
            , ("nvim", myEditor ++ "/home/abjoru/.config/nvim/init.vim", "NeoVim main config file")
            ]

myAppGrid :: [(String, String)]
myAppGrid = [ (a,b) | (a,b,c) <- xs]
  where xs = myApplications

myConfigGrid :: [(String, String)]
myConfigGrid = [ (a,b) | (a,b,c) <- xs]
  where xs = myConfigs

------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------
myXPConfig :: XPConfig
myXPConfig = def
  { font                = "xft:Mononoki Nerd Font:size=9"
  , bgColor             = gruvBG0
  , fgColor             = gruvFG0
  , bgHLight            = gruvBlue0
  , fgHLight            = "#00000"
  , borderColor         = gruvBG3
  , promptBorderWidth   = 0
  , promptKeymap        = defaultXPKeymap
  , position            = Top
  , height              = 20
  , historySize         = 256
  , historyFilter       = id
  , defaultText         = []
  -- , autoComplete	= Just 100000 -- set Just 100000 for .1 sec
  , showCompletionOnTab = False
  --, searchPredicate     = isPrefixOf
  , alwaysHighlight     = True
  , maxComplRows        = Nothing -- set to Just 5 for 5 rows
  }

-- A list of all of the standard Xmonad prompts
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)                 -- manpages prompt
             , ("p", passPrompt)                -- get passwords (requires 'pass')
             , ("g", passGeneratePrompt)        -- generate passwords (requires 'pass')
             , ("r", passRemovePrompt)          -- remove passwords (requires 'pass')
             , ("s", sshPrompt)                 -- ssh prompt
             , ("x", xmonadPrompt)              -- xmonad prompt
             ]

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = 
  inputPrompt c (trim ans) ?+ \input ->
    liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
    trim = f . f
      where f = reverse . dropWhile isSpace

-- Xmonad has several search engines available to use located in
-- XMonad.Actions.Search. Additionally, you can add other search engines
-- such as those listed below.
archwiki, ebay, news, reddit, urban :: S.SearchEngine
archwiki = S.searchEngine "archwiki"    "https://wiki.archlinux.org/index.php?search="
ebay     = S.searchEngine "ebay"        "https://www.ebay.com/sch/i.html?_nkw="
news     = S.searchEngine "news"        "https://news.google.com/search?q="
reddit   = S.searchEngine "reddit"      "https://www.reddit.com/search/?q="
urban    = S.searchEngine "urban"       "https://www.urbandictionary.com/define.php?term="

-- This is the list of search engines that I want to use. Some are from 
-- XMonad.Actions.Search, and some are the ones defined above.
searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("d", S.duckduckgo)
             , ("e", ebay)
             , ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("n", news)
             , ("r", reddit)
             , ("s", S.stackage)
             , ("t", S.thesaurus)
             , ("v", S.vocabulary)
             , ("b", S.wayback)
             , ("u", urban)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("z", S.amazon)
             ]

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
  --, ("M-S-=", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)  -- Toggles noborder/full
  --, ("M-S-f", sendMessage (T.Toggle "float"))
  --, ("M-S-x", sendMessage $ Toggle REFLECTX)
  --, ("M-S-y", sendMessage $ Toggle REFLECTY)
  --, ("M-S-m", sendMessage $ Toggle MIRROR)
  , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))     -- Increase number of clients in the master pane
  , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))    -- Decrease number of clients in the master pane
  , ("M-S-<KP_Multiply", increaseLimit)                 -- Increase number of windows that can be shown
  , ("M-S-<KP_Divide", decreaseLimit)                   -- Decrease number of windows that can be shown

  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)
  --, ("M-S-;", sendMessage zoomReset)
  --, ("M-;", sendMessage ZoomFullToggle)

  -- Workspaces
  , ("M-.", nextScreen)                                                 -- Switch focus to next monitor
  , ("M-,", prevScreen)                                                 -- Switch focus to prev monitor
  , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)         -- Shifts focused window to next workspace
  , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)    -- Shifts focused window to prev workspace

  -- Scratchpads
  , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
  , ("M-C-c", namedScratchpadAction myScratchPads "cmus")

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
    myCFloats  = ["Arandr", "Gimp", "Galculator", "feh", "mpv"]
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
-- Override the PP values as you would otherwise, adding colors etc depending
-- on the statusbar used
myLogHook :: D.Client -> PP
myLogHook dbus = def
  { ppOutput = dbusOutput dbus
  , ppCurrent = wrap ("%{F" ++ gruvGreen0 ++ "} [") "]%{F-}"
  , ppVisible = wrap ("%{F" ++ gruvGreen1 ++ "} ") "%{F-}"
  , ppUrgent  = wrap ("%{F" ++ gruvOrange0 ++ "} ") "%{F-}"
  , ppHidden  = wrap ("%{F" ++ gruvBlue0 ++ "} ") "%{F-}"
  , ppTitle   = wrap ("%{F" ++ gruvGray0 ++ "} ") "%{F-}"
  , ppSep     = " | "
  }

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True
-- This is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall            = renamed [Replace "tall"]
                  $ limitWindows 12
                  $ mySpacing 6
                  $ ResizableTall 1 (3/100) (1/2) []

magnify         = renamed [Replace "magnify"]
                  $ magnifier
                  $ limitWindows 12
                  $ mySpacing 6
                  $ ResizableTall 1 (3/100) (1/2) []

monocle         = renamed [Replace "monocle"]
                  $ limitWindows 20
                  $ Full

floats          = renamed [Replace "floats"]
                  $ limitWindows 20
                  $ simplestFloat

grid            = renamed [Replace "grid"]
                  $ limitWindows 12
                  $ mySpacing 6
                  $ mkToggle (single MIRROR)
                  $ Grid (16/10)

spirals         = renamed [Replace "spirals"]
                  $ mySpacing' 6
                  $ spiral (6/7)

threeCol        = renamed [Replace "threeCol"]
                  $ limitWindows 7
                  $ mySpacing' 4
                  $ ThreeCol 1 (3/100) (1/2)

threeRow        = renamed [Replace "threeRow"]
                  $ limitWindows 7
                  $ mySpacing' 4
                  -- Mirror takes a layout and rotates it by 90 degrees.
                  -- So we are applying Mirror to the ThreeCol layout.
                  $ Mirror
                  $ ThreeCol 1 (3/100) (1/2)
tabs            = renamed [Replace "tabs"]
                  -- I cannot add spacing to this layout because it will
                  -- add spacing between window and tabs which looks bad
                  $ tabbed shrinkText myTabConfig
                  where myTabConfig = def { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=11"
                                          , activeColor         = "#292d3e"
                                          , inactiveColor       = "#3e445e"
                                          , activeBorderColor   = "#292d3e"
                                          , inactiveBorderColor = "#292d3e"
                                          , activeTextColor     = "#ffffff"
                                          , inactiveTextColor   = "#d0d0d0"
                                          }

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font 	= "xft:Sans:bold:size=60"
  , swn_fade 	= 1.0
  , swn_bgcolor = "#000000"
  , swn_color 	= "#FFFFFF"
  }

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
	       mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
	where
	  myDefaultLayout = tall ||| magnify ||| noBorders monocle ||| floats ||| noBorders tabs

------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
		, NS "cmus" spawnCmus findCmus manageCmus
		]
	where
	spawnTerm  = myTerminal ++ " -n scratchpad"
	findTerm   = resource =? "scratchpad"
	manageTerm = customFloating $ W.RationalRect l t w h
		where
		h = 0.9
		w = 0.9
		t = 0.95 -h
		l = 0.95 -w
	spawnCmus  = myTerminal ++ " -n cmus 'cmus'"
	findCmus   = resource =? "cmus"
	manageCmus = customFloating $ W.RationalRect l t w h
		where
		h = 0.9
		w = 0.9
		t = 0.95 -h
		l = 0.95 -w

------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  -- The xmonad, ya know...what the wm is named after.
  xmonad $ ewmh $ docks $ defaults { logHook = dynamicLogWithPP (myLogHook dbus) }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal objectPath interfaceName memberName) {
    D.signalBody = [D.toVariant $ UTF8.decodeString str]
  }
  D.emit dbus signal
  where
    objectPath    = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName 	  = D.memberName_ "Update"

defaults = def
  { handleEventHook 	= serverModeEventHookCmd
			  <+> serverModeEventHook
			  <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
			  <+> docksEventHook
  , modMask 		= myModMask
  , terminal 		= myTerminal
  , workspaces 		= myWorkspaces
  , layoutHook 		= showWName' myShowWNameTheme $ smartBorders $ myLayoutHook
  , normalBorderColor 	= myNormColor
  , focusedBorderColor 	= myFocusColor
  , manageHook 		= myManageHook <+> manageHook def
  , borderWidth 	= myBorderWidth
  , startupHook 	= myStartupHook
  } `additionalKeysP` myKeys
