module Blueberry.Variables where

import Blueberry.Palette

import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, PP(..))
import qualified XMonad.Actions.Search as S
import qualified XMonad.StackSet as W

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

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
myNormColor = pBG0

-- border color of focused windows
myFocusColor :: [Char]
myFocusColor = pFG1

-- setting this for use in xprompts
altMask :: KeyMask
altMask = mod1Mask

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myDBusHook :: D.Client -> PP
myDBusHook dbus = def
  { ppOutput = dbusOutput dbus
  , ppCurrent = wrap ("%{F" ++ pGreen0 ++ "} [") "]%{F-}"
  , ppVisible = wrap ("%{F" ++ pGreen1 ++ "} ") "%{F-}"
  , ppUrgent  = wrap ("%{F" ++ pOrange0 ++ "} !") "%{F-}"
  , ppHidden  = wrap ("%{F" ++ pBlue0 ++ "} ") "%{F-}"
  , ppTitle   = wrap ("%{F" ++ pGray0 ++ "} ") "%{F-}"
  , ppSep     = " | "
  }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal objectPath interfaceName memberName) { D.signalBody = [D.toVariant $ UTF8.decodeString str] }
  D.emit dbus signal
  where
    objectPath    = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName    = D.memberName_ "Update"

------------------------------------------------------------------------
-- Search Variables
------------------------------------------------------------------------

searchArchwiki, searchEbay, searchReddit, searchUrban :: S.SearchEngine
searchArchwiki  = S.searchEngine "archwiki"     "https://wiki.archlinux.org/index.php?search="
searchEbay      = S.searchEngine "ebay"         "https://www.ebay.com/sch/i.html?_nkw="
searchReddit    = S.searchEngine "reddit"       "https://www.reddit.com/search/?q="
searchUrban     = S.searchEngine "urban"        "https://www.urbandictionary.com/define.php?term="

searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", searchArchwiki)
             , ("e", searchEbay)
             , ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("r", searchReddit)
             , ("s", S.stackage)
             , ("t", S.thesaurus)
             , ("v", S.vocabulary)
             , ("b", S.wayback)
             , ("u", searchUrban)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("z", S.amazon)
             ]

------------------------------------------------------------------------
-- Application/Config Variables
------------------------------------------------------------------------

-- The lists below are actually 3-tuples for use with gridSelect and treeSelect.
-- TreeSelect uses all three values in the 3-tuples but GridSelect only needs first
-- two values in each list
myApplications :: [(String, String, String)]
myApplications = [ ("QuteBrowser", "qutebrowser", "Simple VIM-like web browser")
                 , ("Firefox", "firefox", "The famous open source web browser")
                 , ("PCManFM", "pcmanfm", "Lightweight graphical file manager")
                 , ("ThinkOrSwim", "thinkorswim", "TD Ameritrade platform")
                 , ("Steam", "steam", "Proprietary gaming platform")
                 , ("PrusaSlicer", "prusa-slicer", "Prusa 3d printer slicer software")
                 , ("VirtualBox", "virtualbox", "Virtualization software")
                 , ("Blender", "blender", "Blender 3D Software")
                 , ("Geary", "geary", "Geary Email Client")
                 , ("Gitter", "gitter", "Gitter Desktop Client")
                 ]

myConfigs :: [(String, String, String)]
myConfigs = [ ("xmonad", myEditor ++ "/home/abjoru/.config/xmonad/xmonad.hs", "xmonad config")
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
