-- The xmonad configuration

------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------
  -- Base
import XMonad
import XMonad.Config.Desktop
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

  -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

  -- Data
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import qualified Data.Map as M

  -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

  -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doFullFloat, doCenterFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.

  -- Actions
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.Search as S

  -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect (REFLECTX(..), REFLECTY(..))
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

  -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.OneBig
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ZoomRow (zoomRow, zoomReset, ZoomMessage(ZoomFullToggle))

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
--myTerminal = "alacritty" 
myTerminal = "termonad"

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
  --(0x31,0x2e,0x39) -- lowest inactive bg 	RGB: (49, 46, 57) 	GRUV: #1d2021 (29, 32, 33)
  (0x1d,0x20,0x21)
  --(0x31,0x2e,0x39) -- highest inactive bg 	RGB: (49, 46, 57) 	GRUV: #282828 (40, 40, 40)
  (0x28,0x28,0x28)
  --(0x61,0x57,0x72) -- active bg 		RGB: (97, 87, 114)      GRUV: #665c54 (102, 92, 84)
  (0x66,0x5c,0x54)
  --(0xc0,0xa7,0x9a) -- inactive fg 		RGB: (192, 167, 154) 	GRUV: #a89984 (168, 153, 132)
  (0xa8,0x99,0x84)
  --(0xff,0xff,0xff) -- active fg 		RGB: (255, 255, 255) 	GRUV: #fbf1c7 (251, 241, 199)
  (0xfb,0xf1,0xc7)

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

------------------------------------------------------------------------
-- XPROMPT KEYMAP (emacs-like bindings for now..)
------------------------------------------------------------------------
dtXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
dtXPKeymap = M.fromList $
  map (first $ (,) controlMask) 	-- control + <key>
  [ (xK_z, killBefore)			-- kill line backwards
  , (xK_k, killAfter)			-- kill line forwards
  , (xK_a, startOfLine)			-- move to the beginning of the line
  , (xK_e, endOfLine)			-- move to the end of the line
  , (xK_m, deleteString Next)		-- delete a character forward
  , (xK_b, moveCursor Prev)		-- move cursor forward
  , (xK_f, moveCursor Next)		-- move cursor backward
  , (xK_BackSpace, killWord Prev)	-- kill the previous word
  , (xK_y, pasteString)			-- paste a string
  , (xK_g, quit)			-- quit out of prompt
  , (xK_bracketleft, quit)
  ]
  ++
  map (first $ (,) altMask)		-- meta key + <key>
  [ (xK_BackSpace, killWord Prev)	-- kill the prev word
  , (xK_f, moveWord Next)		-- move a word forward
  , (xK_b, moveWord Prev)		-- move a word backward
  , (xK_d, killWord Next)		-- kill the next word
  , (xK_n, moveHistory W.focusUp')	-- move up thru history
  , (xK_p, moveHistory W.focusDown')	-- move down thru history
  ]
  ++
  map (first $ (,) 0) -- <key>
  [ (xK_Return, setSuccess True >> setDone True)
  , (xK_KP_Enter, setSuccess True >> setDone True)
  , (xK_BackSpace, deleteString Prev)
  , (xK_Delete, deleteString Prev)
  , (xK_Left, moveCursor Prev)
  , (xK_Right, moveCursor Next)
  , (xK_Home, startOfLine)
  , (xK_End, endOfLine)
  , (xK_Down, moveHistory W.focusUp')
  , (xK_Up, moveHistory W.focusDown')
  , (xK_Escape, quit)
  ]

------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------
dtXPConfig :: XPConfig
dtXPConfig = def
  { font		= "xft:Mononoki Nerd Font:size=9"
  , bgColor 		= gruvBG0
  , fgColor 		= gruvFG0
  , bgHLight 		= gruvBlue0
  , fgHLight		= "#00000"
  , borderColor 	= gruvBG3
  , promptBorderWidth	= 0
  , promptKeymap	= dtXPKeymap
  , position		= Top
  , height		= 20
  , historySize		= 256
  , historyFilter	= id
  , defaultText		= []
  , autoComplete	= Just 100000 -- set Just 100000 for .1 sec
  , showCompletionOnTab	= False
  , searchPredicate	= isPrefixOf
  , alwaysHighlight	= True
  , maxComplRows	= Nothing -- set to Just 5 for 5 rows
  }

-- The same config minus the autocomplete flag which is annoying on
-- certain Xprompts, like the search engine prompts.
dtXPConfig' :: XPConfig
dtXPConfig' = dtXPConfig
  { autoComplete = Nothing }

-- A list of all of the standard Xmonad prompts
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt) 		-- manpages prompt
	     , ("p", passPrompt) 		-- get passwords (requires 'pass')
	     , ("g", passGeneratePrompt) 	-- generate passwords (requires 'pass')
	     , ("r", passRemovePrompt) 		-- remove passwords (requires 'pass')
	     , ("s", sshPrompt) 		-- ssh prompt
	     , ("x", xmonadPrompt) 		-- xmonad prompt
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
archwiki = S.searchEngine "archwiki" 	"https://wiki.archlinux.org/index.php?search="
ebay 	 = S.searchEngine "ebay" 	"https://www.ebay.com/sch/i.html?_nkw="
news 	 = S.searchEngine "news" 	"https://news.google.com/search?q="
reddit 	 = S.searchEngine "reddit" 	"https://www.reddit.com/search/?q="
urban 	 = S.searchEngine "urban" 	"https://www.urbandictionary.com/define.php?term="

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
  [ ("M-C-r", spawn "xmonad --recompile")		-- Recompiles xmonad
  , ("M-S-r", spawn "xmonad --restart")			-- Restarts xmonad
  , ("M-S-q", io exitSuccess)				-- Quits xmonad
  , ("M1-S-q", io exitSuccess)				-- Quits xmonad

  -- Prompts
  , ("M-S-<Return>", shellPrompt dtXPConfig)		-- Shell prompt

  -- Windows
  , ("M-S-c", kill1)					-- Kill the currently focused client
  , ("M-S-a", killAll)					-- Kill all the windows on current workspace

  -- Floating windows
  , ("M-<Delete>", withFocused $ windows . W.sink)	-- Push floating window back to tile
  , ("M-S-<Delete>", sinkAll)				-- Push all floating windows back to tile

  -- Grid Select
  --, ("M-S-t", spawnSelected' myAppGrid) 		-- grid select favirite apps
  , ("M-S-g", goToSelected $ myGridConfig myColorizer)	-- goto selected
  , ("M-S-b", bringSelected $ myGridConfig myColorizer)	-- bring selected

  -- Window navigation
  , ("M-m", windows W.focusMaster)			-- Move focus to the master window
  , ("M-j", windows W.focusDown)			-- Move focus to the next window
  , ("M-k", windows W.focusUp)				-- Move focus to the prev window
  , ("M-S-j", windows W.swapDown)			-- Swap the focused window with the next window
  , ("M-S-k", windows W.swapUp)				-- Swap the focused window with the prev window
  , ("M-<Backspace>", promote)				-- Moves focused window to master, all others maintain order
  , ("M1-S-<Tab>", rotSlavesDown)			-- Rotate all windows except master and keep focus in place
  , ("M1-C-<Tab>", rotAllDown)				-- Rotate all the windows in the current stack
  , ("M-C-s", killAllOtherCopies)

  , ("M-C-M1-<Up>", sendMessage Arrange)
  , ("M-C-M1-<Down>", sendMessage DeArrange)
  , ("M-<Up>", sendMessage (MoveUp 10))			-- Move focused window to up
  , ("M-<Down>", sendMessage (MoveDown 10))		-- Move focused window to down
  , ("M-<Right>", sendMessage (MoveRight 10))		-- Move focused window to right
  , ("M-<Left>", sendMessage (MoveLeft 10))		-- Move focused window to left
  , ("M-S-<Up>", sendMessage (IncreaseUp 10))		-- Increase size of focused window up
  , ("M-S-<Down>", sendMessage (IncreaseDown 10))	-- Increase size of focused window down
  , ("M-S-<Right>", sendMessage (IncreaseRight 10))	-- Increase size of focused window right
  , ("M-S-<Left>", sendMessage (IncreaseLeft 10))	-- Increase size of focused window left
  , ("M-C-<Up>", sendMessage (DecreaseUp 10))		-- Decrease size of focused window up
  , ("M-C-<Down>", sendMessage (DecreaseDown 10))	-- Decrease size of focused window down
  , ("M-C-<Right>", sendMessage (DecreaseRight 10))	-- Decrease size of focused window right
  , ("M-C-<Left>", sendMessage (DecreaseLeft 10))	-- Decrease size of focused window left

  -- Layouts
  , ("M-<Tab>", sendMessage NextLayout)					-- Switch to next layout
  , ("M-S-<Space>", sendMessage ToggleStruts)				-- Toggles struts
  , ("M-S-n", sendMessage $ Toggle NOBORDERS)				-- Toggles noborder
  , ("M-S-=", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)	-- Toggles noborder/full
  , ("M-S-f", sendMessage (T.Toggle "float"))				
  , ("M-S-x", sendMessage $ Toggle REFLECTX)
  , ("M-S-y", sendMessage $ Toggle REFLECTY)
  --, ("M-S-m", sendMessage $ Toggle MIRROR)
  , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))	-- Increase number of clients in the master pane
  , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))	-- Decrease number of clients in the master pane
  , ("M-S-<KP_Multiply", increaseLimit)			-- Increase number of windows that can be shown
  , ("M-S-<KP_Divide", decreaseLimit)			-- Decrease number of windows that can be shown

  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)
  , ("M-S-;", sendMessage zoomReset)
  , ("M-;", sendMessage ZoomFullToggle)

  -- Workspaces
  , ("M-.", nextScreen)							-- Switch focus to next monitor
  , ("M-,", prevScreen)							-- Switch focus to prev monitor
  , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)		-- Shifts focused window to next workspace
  , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)	-- Shifts focused window to prev workspace

  -- Scratchpads
  , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
  , ("M-C-c", namedScratchpadAction myScratchPads "cmus")

  -- Open My Preffered Terminal.
  , ("M-<Return>", spawn myTerminal)
  , ("M1-<Return>", spawn myTerminal)

  -- Dmenu Scripts (Alt+Ctrl+Key)
  --, ("M-S-<Return>", spawn "dmenu_run")
  , ("M1-C-e", spawn "/home/abjoru/.config/dmenu/dmenu-edit-configs.sh")
  --, ("M1-C-h", spawn "./.dmenu/dmenu-hugo.sh")
  --, ("M1-C-m", spawn "./.dmenu/dmenu-sysmon.sh")
  --, ("M1-C-s", spawn "./.dmenu/dmenu-surfraw.sh")
  --, ("M1-C-/", spawn "./.dmenu/dmenu-scrot.sh")

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
  -- ++ [("M-s " ++ k, S.promptSearch dtXPConfig' f) | (k,f) <- searchList ]
  -- ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
  -- ++ [("M-p " ++ k, f dtXPConfig') | (k,f) <- promptList ]
  -- ++ [("M-p " ++ k, f dtXPConfig' g) | (k,f,g) <- promptList' ]
  -- Appending named scratchpads to keybinding list
    where nonNSP		= WSIs (return (\ws -> W.tag ws /= "nsp"))
  	  nonEmptyNonNSP	= WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

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
myWorkspaces = clickable . (map xmobarEscape)
	       $ ["dev", "www", "sys", "doc", "vbox", "chat", "mus", "vid", "gfx"]
  where
    clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                  (i,ws) <- zip [1..9] l,
		  let n = i ]

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
--    , [className =? c --> doShift (myWorkspaces !! 0) <+> viewShift (myWorkspaces !! 0)        | c <- my1Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 1) <+> viewShift (myWorkspaces !! 1)        | c <- my2Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 2) <+> viewShift (myWorkspaces !! 2)        | c <- my3Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 3) <+> viewShift (myWorkspaces !! 3)        | c <- my4Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 4) <+> viewShift (myWorkspaces !! 4)        | c <- my5Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 5) <+> viewShift (myWorkspaces !! 5)        | c <- my6Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 6) <+> viewShift (myWorkspaces !! 6)        | c <- my7Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 7) <+> viewShift (myWorkspaces !! 7)        | c <- my8Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 8) <+> viewShift (myWorkspaces !! 8)        | c <- my9Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 9) <+> viewShift (myWorkspaces !! 9)        | c <- my10Shifts]
  ]
  where
    myCFloats = ["Arandr", "Gimp", "Galculator", "feh", "mpv"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIFloats = ["desktop_window"]
  -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
  -- I'm doing it this way because otherwise I would have to write out
  -- the full name of my clickable workspaces, which would look like:
  -- doShift "<action xdotool super+8>gfx</action>"
  --[ className =? "obs"		--> doShift ( myWorkspaces !! 7)
  --, title =? "firefox"		--> doShift ( myWorkspaces !! 1)
  --, title =? "qutebrowser"	--> doShift ( myWorkspaces !! 1)
  --, className =? "mpv"		--> doShift ( myWorkspaces !! 7)
  --, className =? "vlc"		--> doShift ( myWorkspaces !! 7)
  --, className =? "Gimp"		--> doShift ( myWorkspaces !! 8)
  --, className =? "Gimp"		--> doFloat
  --, title =? "Oracle VM VirtualBox Manager"	--> doFloat
  --, className =? "Oracle VM VirtualBox Manager"	--> doShift ( myWorkspaces !! 6)
  --, (className =? "firefox" <&&> resource =? "Dialog")	--> doFloat -- Float Firefox Dialog
  --] <+> namedScratchpadManageHook myScratchPads

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

tall		= renamed [Replace "tall"]
		  $ limitWindows 12
		  $ mySpacing 6
		  $ ResizableTall 1 (3/100) (1/2) []

monocle		= renamed [Replace "monocle"]
		  $ limitWindows 20
		  $ Full

floats		= renamed [Replace "floats"]
		  $ limitWindows 20
		  $ simplestFloat

grid		= renamed [Replace "grid"]
		  $ limitWindows 12
		  $ mySpacing 6
		  $ mkToggle (single MIRROR)
		  $ Grid (16/10)

spirals		= renamed [Replace "spirals"]
		  $ mySpacing' 6
		  $ spiral (6/7)

threeCol	= renamed [Replace "threeCol"]
		  $ limitWindows 7
		  $ mySpacing' 4
		  $ ThreeCol 1 (3/100) (1/2)

threeRow	= renamed [Replace "threeRow"]
		  $ limitWindows 7
		  $ mySpacing' 4
		  -- Mirror takes a layout and rotates it by 90 degrees.
		  -- So we are applying Mirror to the ThreeCol layout.
		  $ Mirror
		  $ ThreeCol 1 (3/100) (1/2)

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
	       mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
	where
	  myDefaultLayout = tall ||| noBorders monocle ||| floats ||| grid ||| spirals ||| threeCol ||| threeRow

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
	-- Launching three instances of xmobar on their monitors.
	xmproc <- spawnPipe "xmobar -x 0 /home/abjoru/.config/xmobar/xmobarrc0"
	--xmproc0 <- spawnPipe "xmobar -x 0 /home/abjoru/.config/xmobar/xmobarrc0"
	--xmproc1 <- spawnPipe "xmobar -x 1 /home/abjoru/.config/xmobar/xmobarrc1"
	--xmproc2 <- spawnPipe "xmobar -x 2 /home/abjoru/.config/xmobar/xmobarrc2"
	
	-- xmonad stuff
	xmonad $ ewmh desktopConfig
		{ manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
		, modMask 		= myModMask
		, terminal		= myTerminal
		, startupHook		= myStartupHook
		, layoutHook		= myLayoutHook
		, workspaces		= myWorkspaces
		, borderWidth		= myBorderWidth
		, normalBorderColor	= myNormColor
		, focusedBorderColor	= myFocusColor
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = \x -> hPutStrLn xmproc x -- \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x
			, ppCurrent = xmobarColor gruvGreen0 "" . wrap "[" "]"	-- Current workspace in xmobar
			, ppVisible = xmobarColor gruvGreen1 ""			-- Visible but not current workspace
			, ppHidden = xmobarColor gruvBlue0 "" . wrap "*" ""	-- Hidden workspaces in xmobar
			, ppHiddenNoWindows = xmobarColor gruvRed0 ""		-- Hidden workspaces (no windows)
			, ppTitle = xmobarColor gruvGray0 "" . shorten 60	-- Title of active window in xmobar
			, ppSep = "<fc=#666666> | </fc>"			-- Separators in xmobar
			, ppUrgent = xmobarColor gruvOrange0 "" . wrap "!" "!"	-- Urgent workspace
			, ppExtras = [windowCount]				-- # of windows in current workspace
			, ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
			}
		} `additionalKeysP` myKeys
