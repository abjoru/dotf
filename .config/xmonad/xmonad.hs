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
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
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
-- VARIABLES
------------------------------------------------------------------------
myFont :: [Char]
myFont = "xft:Mononoki Nerd Font:regular:pixelsize=11"

-- sets modkey to super/windows key
myModMask :: KeyMask
myModMask = mod4Mask 

-- sets default terminal
myTerminal :: [Char]
myTerminal = "alacritty" 

-- sets border width for windows
myBorderWidth :: Dimension
myBorderWidth = 2

-- border color of normal windows
myNormColor :: [Char]
myNormColor = "#292d3e"

-- border color of focused windows
myFocusColor :: [Char]
myFocusColor = "$bbc5ff"

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
  spawnOnce "nitrogen --restore &"
  --spawnOnce "picom &"
  spawnOnce "compton --config /home/abjoru/.config/compton/compton.conf &"
  --spawnOnce "nm-applet &"
  --spawnOnce "volumeicon &"
  --spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDocType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x292d3e --height 18 &"
  --spawnOnce "kak -d -s mysession &"
  setWMName "Blueberry"

------------------------------------------------------------------------
-- GRID SELECT
------------------------------------------------------------------------
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
  (0x31,0x2e,0x39) -- lowest inactive bg
  (0x31,0x2e,0x39) -- highest inactive bg
  (0x61,0x57,0x72) -- active bg
  (0xc0,0xa7,0x9a) -- inactive fg
  (0xff,0xff,0xff) -- active fg

-- gridSelect menu layout
myGridConfig :: p -> GSConfig Window
myGridConfig colorizer = (buildDefaultGSConfig myColorizer)
  { gs_cellheight = 30
  , gs_cellwidth = 200
  , gs_cellpadding = 8
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
  , bgColor		= "#292d3e"
  , fgColor		= "#d0d0d0"
  , bgHLight		= "c792ea"
  , fgHLight		= "#00000"
  , borderColor		= "#535974"
  , promptBorderWidth	= 0
  , promptKeymap	= dtXPKeymap
  , position		= Top
--  , position		= CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
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

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = 
  inputPrompt c (trim ans) ?+ \input ->
    liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
    trim = f . f
      where f = reverse . dropWhile isSpace

------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------
myKeys :: [([Char], X ())]
myKeys =
  -- Xmonad
  [ ("M-C-r", spawn "xmonad --recompile")		-- Recompiles xmonad
  , ("M-S-r", spawn "xmonad --restart")			-- Restarts xmonad
  , ("M-S-q", io exitSuccess)				-- Quits xmonad

  -- Prompts
  , ("M-S-<Return>", shellPrompt dtXPConfig)		-- Shell prompt
  , ("M-S-o", xmonadPrompt dtXPConfig)			-- Xmonad prompt
  , ("M-S-s", sshPrompt dtXPConfig)			-- Ssh prompt
  , ("M-S-m", manPrompt dtXPConfig)			-- Manpage prompt
  -- Requires pass to be installed
  , ("M1-C-p", passPrompt dtXPConfig)			-- Get passwords prompt
  , ("M1-C-g", passGeneratePrompt dtXPConfig)		-- Generate passwords prompt
  , ("M1-C-r", passRemovePrompt dtXPConfig)		-- Remove passwords prompt
  -- Calculator prompt
  , ("M1-C-c", calcPrompt dtXPConfig "qalc")		-- Requires qalculate-gtk

  -- Windows
  , ("M-S-c", kill1)					-- Kill the currently focused client
  , ("M-S-a", killAll)					-- Kill all the windows on current workspace

  -- Floating windows
  , ("M-<Delete>", withFocused $ windows . W.sink)	-- Push floating window back to tile
  , ("M-S-<Delete>", sinkAll)				-- Push all floating windows back to tile

  -- Grid Select
  , (("M-S-t"), spawnSelected'
    [ ("Audacity", "audacity")
    , ("Deadbeef", "deadbeef")
    , ("Firefox", "firefox")
    , ("Geany", "geany")
    , ("Geary", "geary")
    , ("Gimp", "gimp")
    , ("Kdenlive", "kdenlive")
    , ("LibreOffice Impress", "loimpress")
    , ("LibreOffice Writer", "lowriter")
    , ("OBS", "obs")
    , ("PCManFM", "pcmanfm")
    , ("Simple Terminal", "st")
    , ("Stream", "steam")
    , ("Surf Browser", "surf suckless.org")
    , ("Xonotic", "xonotic-glx")
    ])
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

  -- Dmenu Scripts (Alt+Ctrl+Key)
  --, ("M-S-<Return>", spawn "dmenu_run")
  , ("M1-C-e", spawn "./.dmenu/dmenu-edit-configs.sh")
  , ("M1-C-h", spawn "./.dmenu/dmenu-hugo.sh")
  , ("M1-C-m", spawn "./.dmenu/dmenu-sysmon.sh")
  , ("M1-C-s", spawn "./.dmenu/dmenu-surfraw.sh")
  , ("M1-C-/", spawn "./.dmenu/dmenu-scrot.sh")

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
  , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
  , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
  , ("<XF86Eject>", spawn "toggleeject")
  , ("<Print>", spawn "scrotd 0")
  ] where nonNSP		= WSIs (return (\ws -> W.tag ws /= "nsp"))
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

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
  -- I'm doing it this way because otherwise I would have to write out
  -- the full name of my clickable workspaces, which would look like:
  -- doShift "<action xdotool super+8>gfx</action>"
  [ className =? "obs"		--> doShift ( myWorkspaces !! 7)
  , title =? "firefox"		--> doShift ( myWorkspaces !! 1)
  , title =? "qutebrowser"	--> doShift ( myWorkspaces !! 1)
  , className =? "mpv"		--> doShift ( myWorkspaces !! 7)
  , className =? "vlc"		--> doShift ( myWorkspaces !! 7)
  , className =? "Gimp"		--> doShift ( myWorkspaces !! 8)
  , className =? "Gimp"		--> doFloat
  , title =? "Oracle VM VirtualBox Manager"	--> doFloat
  , className =? "Oracle VM VirtualBox Manager"	--> doShift ( myWorkspaces !! 6)
  , (className =? "firefox" <&&> resource =? "Dialog")	--> doFloat -- Float Firefox Dialog
  ] <+> namedScratchpadManageHook myScratchPads

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
		  $ mySpacing 8
		  $ ResizableTall 1 (3/100) (1/2) []

monocle		= renamed [Replace "monocle"]
		  $ limitWindows 20
		  $ Full

floats		= renamed [Replace "floats"]
		  $ limitWindows 20
		  $ simplestFloat

grid		= renamed [Replace "grid"]
		  $ limitWindows 12
		  $ mySpacing 8
		  $ mkToggle (single MIRROR)
		  $ Grid (16/10)

spirals		= renamed [Replace "spirals"]
		  $ mySpacing' 8
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
			, ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]"	-- Current workspace in xmobar
			, ppVisible = xmobarColor "#c3e88d" ""			-- Visible but not current workspace
			, ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""	-- Hidden workspaces in xmobar
			, ppHiddenNoWindows = xmobarColor "#F07178" ""		-- Hidden workspaces (no windows)
			, ppTitle = xmobarColor "#d0d0d0" "" . shorten 60	-- Title of active window in xmobar
			, ppSep = "<fc=#666666> | </fc>"			-- Separators in xmobar
			, ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"	-- Urgent workspace
			, ppExtras = [windowCount]				-- # of windows in current workspace
			, ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
			}
		} `additionalKeysP` myKeys
