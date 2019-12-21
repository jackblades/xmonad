{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
module Main (main) where

-- TODO dynamic workspaces
-- import XMonad.Prompt.AppendFile
-- import XMonad.Prompt.FuzzyMatch
-- import XMonad.Actions.DynamicWorkspaces
-- import XMonad.Actions.CopyWindow(copy)

------------------------------------------------------------------------------
-- XMonad.Actions.FloatSnap
import qualified Data.Tree as Tree
import qualified Data.Monoid as Monoid

import Control.Monad (replicateM_, when, join)
import System.Exit (exitSuccess)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (dynamicLogString, xmonadPropLog)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode (serverModeEventHookCmd')
import XMonad.Layout.Fullscreen (fullscreenManageHook)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.MultiToggle as MT -- (Toggle, Transformer, transform)
import XMonad.Layout.MultiToggle.Instances as MTI (StdTransformers(..))
import XMonad.Layout.Spacing -- (spacingRaw, Border)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.AppLauncher (launchApp)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

import qualified XMonad.StackSet as W
import qualified XMonad.Layout.Magnifier as Magnifier

import QsrTile
import QsrFloatTile
import QsrFloat
import XDecoration
import XState
import PositionStore
import Util



-- | Colorize a window depending on it's className.
-- fromClassName :: Window -> Bool -> X ()
-- fromClassName w active = runQuery className w >>= undefined

-- runQuery :: Query a -> Window -> X a
-- (=?) :: Eq a => Query a -> a -> Query Bool
-- (-?>) :: Query Bool -> ManageHook -> MaybeManageHook
-- className =? "kitty" :: Query Bool
--           -?> doFloat

------------------------------------------------------------------------------
-- | xmonad server client cmds
cmds :: [(String, X ())]
cmds =
  [ ("terminalToggle", terminalToggle)
  , ("apps", rofiApps)
  , ("calendar", spawn "/etc/settings-calendar")
  , ("network", spawn "/etc/settings-network")
  , ("power", spawn "/etc/settings-power")
  , ("sound", spawn "/etc/settings-sound")
  , ("colorinvert", spawn "xcalib -i -a")
  , ("layout", sendMessage NextLayout)
  , ("kill", kill)
  , ("layout-maximize", sendMessage (MT.Toggle MAXIMIZE))
  , ("layout-full", sendMessage (MT.Toggle FULL) >> raiseFocusedWindow)
  , ("ws-next", moveTo Next NonEmptyWS)
  , ("ws-prev", moveTo Prev NonEmptyWS)
  , ("win-next", windows W.focusUp)
  , ("win-prev", windows W.focusDown)
  ]

rofiApps = spawn "rofi -show drun -config /etc/rofi/runconfig"
screenshot = spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -d 1-u -e 'mv $f /media/external/quasar/screenshot/'"
------------------------------------------------------------------------------
-- | setup client/server and args
main :: IO ()
main = getArgs >>= \case
  [] -> wmserver
  "client":c:args -> sendCommand c
  _ -> return ()

-- xmonad client
wmserver :: IO ()
wmserver = do
  spawn "systemctl --user restart quasar-topbar"
  spawn "systemctl --user restart quasar-compton"

  let config = rootConfig `additionalKeysP` keyConfig
  -- ewmh is needed to get window info in bar
  xmonad $ ewmh config

-- xmonad server
rootConfig = desktopConfig
  { modMask = mod4Mask -- win key
  -- window borders
  , borderWidth = 0
  , focusedBorderColor = "#000000"
  , focusFollowsMouse = False
  -- xmonad startup
  , startupHook = do
      spawnOnce "/etc/xautolock-locker"
      addEWMHFullscreen
      startupHook desktopConfig
  , layoutHook = myLayouts
  -- on create window
  , manageHook = fullscreenManageHook
      <+> manageSpawn
      <+> myManageHook
      <+> manageHook desktopConfig
  -- on windowset change (focus change)
  , logHook = do
      dynamicLogString def >>= xmonadPropLog
      raiseFocusedIfFloating
  -- on general event
  , handleEventHook = fullscreenEventHook
      <+> docksEventHook
      <+> serverModeEventHookCmd' (return cmds)
      <+> handleEventHook def
  }

------------------------------------------------------------------------------
-- | Customize layouts.
myLayouts = spaced 3
  $ mkToggle (FULL ?? MAXIMIZE ?? GRID ?? EOT)
  $ avoidStruts
  $ mkToggle (GRID ?? EOT)
  $ decorateTile qsrFloatTile
    ||| decorateFloat qsrFloat 
    ||| decorateTile qsrGTile
  where
    spaced d = spacingRaw True (Border d d d d) True (Border d d d d) True
    
    -- add to myLayouts otherwise type ambiguity
    -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Layout-Combo.html
    -- twopane = combineTwo (TwoPane 0.03 0.5) (tabbed (shrinkText :: DefaultShrinker) defaultThemeWithButtons) (tabbed (shrinkText :: DefaultShrinker) defaultThemeWithButtons)
    -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Layout-ComboP.html

-- custom toggles
data GRID = GRID deriving (Read, Show, Eq, Typeable)
instance Transformer GRID Window where
  transform GRID x k = k (avoidStruts l) (const x) where
    l = GridRatio (2/2) ||| threeColumns ||| threecMaster ||| threecSlave
    threeColumns = ThreeColMid 1 (3/100) (1/2)
    threecMaster = ThreeColMid 1 (3/100) (6/8)
    threecSlave = ThreeColMid 1 (3/100) (3/8)

data MAXIMIZE = MAXIMIZE deriving (Read, Show, Eq, Typeable)
instance Transformer MAXIMIZE Window where
  transform MAXIMIZE x k = k (avoidStruts Full) (const x)


------------------------------------------------------------------------------
-- | Manipulate windows as they are created.
-- @composeOne@ processes first-match from top to bottom
-- Use the `xprop' tool to get the info you need for these matches.
myManageHook = composeOne
  [ className =? "Tilda" -?> doFloat
  -- , className =? "kitty" -?> doFullFloat
  , className =? "quasar-terminal-kitty" -?> quasarTerminalKittyHook
  , appName =? "Terminator Preferences" -?> doFloat
  , className =? ".terminator-wrapped" -?> rectFloat 0 0 1 1
  -- control applications
  , className =? "Pavucontrol" -?> rectFloat 0.44 0.04 0.55 0.5
  , className =? ".blueman-manager-wrapped" -?> rectFloat 0.23 0.04 0.20 0.5
  , className =? "Lxappearance" -?> rectFloat 0.01 0.2 0.4 0.6
  , className =? "Nm-connection-editor" -?> rectFloat 0.02 0.03 0.2 0.4
  , className =? ".arandr-wrapped" -?> rectFloat 0.51 0.2 0.4 0.6
  , className =? "Wicd-client.py" -?> rectFloat 0.59 0.2 0.4 0.6
  -- generic actions
  , isFullscreen -?> doFullFloat
  , isDialog -?> doCenterFloat
  -- Move transient windows to their parent:
  , transience
  ] where
      fullscreenRect = rectFloat 0 0 1 1
      rectFloat x y w h = doRectFloat $ W.RationalRect x y w h

------------------------------------------------------------------------------
-- | Keybindings
keyConfig =
  [ ("M-S-q",   shellPrompt promptConfig)
  -- , ("M-S-q",   confirmPrompt promptConfig "exit" (io exitSuccess))
  , ("M-<Return>", spawn "kitty")

  --
  , ("M-k",   launchApp promptConfig "thunar")  -- open dir in thunar
  , ("M-c",   posStoreClear)
  -- , ("M-k",   appendFilePrompt def "/home/me/NOTES")

  -- fast actions bound to f-keys
  -- , ("<F1>",)   -- used by terminator
  , ("<F2>", sendMessage NextLayout)
  , ("<F3>", sendMessage (MT.Toggle GRID))
  , ("<F4>", kill)
  , ("<F5>", rotAllUp)
  , ("<F6>", windows W.focusDown)
  , ("<F7>", shiftToPrev >> prevWS)
  , ("<F8>", shiftToNext >> nextWS)
  , ("<F9>", moveTo Next NonEmptyWS)
  , ("<F10>", moveTo Next EmptyWS)
  , ("<F12>", rofiApps)

  , ("<Print>", screenshot)
  , ("<F11>", terminalToggle)


  -- mouse actions
  -- , ("M-m", banish UpperLeft)

  -- cycle windows
  , ("M-<Tab>", windows W.focusUp)
  , ("M-S-<Tab>", windows W.focusDown)
  , ("M-`", windows W.focusDown)
  , ("M-S-`", windows W.focusUp)

  -- move windows across workspaces
  , ("M-<Right>", moveTo Next NonEmptyWS)
  , ("M-<Left>", moveTo Prev NonEmptyWS)
  
  , ("M-S-<Right>", windows W.swapUp)
  , ("M-S-<Left>", windows W.swapDown)
  , ("M-S-<Up>", shiftToPrev >> prevWS)
  , ("M-S-<Down>", shiftToNext >> nextWS)
  
  -- manage windows
  , ("M-w",   kill)
  
  , ("M-f", doFullscreen)
  , ("M-d", sendMessage (MT.Toggle MAXIMIZE))
  , ("M-<Up>", sendMessage (MT.Toggle MAXIMIZE))
  , ("M-<Down>", sendMessage (MT.Toggle MAXIMIZE))
  
  -- switch layouts
  , ("M-t", sendMessage (MT.Toggle GRID))
  , ("M-z", sendMessage NextLayout)
  , ("M-x", sendMessage (MT.Toggle MIRROR))
  , ("M-<Esc>", sendMessage (MT.Toggle GRID))

  -- desktop launcher and terminal
  , ("M-<Space>", spawn "rofi -show drun -config /etc/rofi/runconfig")
  , ("M-S-<Space>", spawn "rofi -show window -width 30em -config /etc/rofi/runconfig")
  , ("M-S-<Return>", spawn "pgrep terminator || terminator")

  -- apps
  , ("M-e", spawn "thunar /home/ajit")
  , ("M-S-e", spawn "thunar /media/external")

  -- system commands
  , ("M-l", spawn "xautolock -locknow")

  -- music
  , ("<XF86AudioPlay>", spawn "mpc toggle")
  , ("<XF86AudioStop>", spawn "mpc pause")
  , ("<XF86AudioPrev>", spawn "mpc prev")
  , ("<XF86AudioNext>", spawn "mpc next")

  -- audio
  , ("<XF86AudioRaiseVolume>", spawn "/etc/settings-volume-set +1.5%")
  , ("<XF86AudioLowerVolume>", spawn "/etc/settings-volume-set -1.5%")
  , ("<XF86AudioMute>", spawn "/etc/settings-volume-mute")

  -- display, brightness and color
  , ("M-i", spawn "xcalib -i -a")
  , ("<XF86MonBrightnessUp>", spawn "/etc/settings-brightness +7%")
  , ("<XF86MonBrightnessDown>", spawn "/etc/settings-brightness -7%")

  ]



------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves
-- https://braincrater.wordpress.com/2008/11/29/pimp-your-xmonad-3-prompt/
promptConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:bitstream vera sans mono:bold:size=9:antialias=true"
  , bgColor           = "black"
  , fgColor           = "white"
  , height            = 27
  }

