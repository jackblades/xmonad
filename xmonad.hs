{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
module Main (main) where

-- TODO dynamic workspaces
-- TODO

------------------------------------------------------------------------------
-- XMonad.Actions.FloatSnap
import qualified Data.Tree as Tree
import qualified Data.List as L (find, findIndex, tails)
import qualified Data.Monoid as Monoid

import Control.Monad (replicateM_, when, join, filterM)
import Data.Maybe (maybeToList)
import System.Exit
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
import XMonad.Actions.Search
import XMonad.Actions.Warp (banish, Corner(UpperLeft))
import XMonad.Actions.WindowGo
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows (isFloating)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Fullscreen (fullscreenManageHook)
import XMonad.Layout.Grid
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances as MTI
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing -- (spacingRaw, Border)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties

import XMonad.Prompt.AppLauncher
import XMonad.Prompt.AppendFile
import XMonad.Prompt.FuzzyMatch

import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Util.Themes
-- layoutHook = windowSwitcherDecorationWithButtons (shrinkText :: DefaultShrinker) defaultThemeWithButtons (draggingVisualizer myLayouts)

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)

import qualified XMonad.Core as XM (Query)
import qualified XMonad.Layout.Magnifier as Magnifier
import qualified XMonad.StackSet as W

-- import XMonad.Layout.Combo
-- import XMonad.Layout.Tabbed
-- import XMonad.Layout.TwoPane

import QsrTile
import QsrFloatTile
import QsrFloat
import BorderResize
import FlexibleResize
import XDecoration

-- xmonad user state
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.SpawnOn
import XMonad.Util.XUtils
import System.Process (system)





data UserState = UserState 
  { _terminal :: Maybe (Either Window Window)
  } deriving (Read, Show, Typeable)

instance ExtensionClass UserState where
  initialValue = UserState { _terminal = Nothing }
  extensionType = PersistentExtension  -- survive mod-q

-- | Colorize a window depending on it's className.
-- fromClassName :: Window -> Bool -> X ()
-- fromClassName w active = runQuery className w >>= undefined

-- runQuery :: Query a -> Window -> X a
-- (=?) :: Eq a => Query a -> a -> Query Bool
-- (-?>) :: Query Bool -> ManageHook -> MaybeManageHook
-- className =? "kitty" :: Query Bool
--           -?> doFloat

------------------------------------------------------------------------------
-- custom toggles
data GRID = GRID deriving (Read, Show, Eq, Typeable)
instance Transformer GRID Window where
  transform GRID x k = k (GridRatio (2/2)) (const x)

data MAXIMIZE = MAXIMIZE deriving (Read, Show, Eq, Typeable)
instance Transformer MAXIMIZE Window where
  transform MAXIMIZE x k = k Full (const x)


data QTILE = QTILE deriving (Read, Show, Eq, Typeable)
instance Transformer QTILE Window where
  transform QTILE x k = k l' (const x) where
    l' = qsrGTile ||| qsr3Tile
    l = threeColumns ||| threecMaster ||| threecSlave
    threeColumns = ThreeColMid 1 (3/100) (1/2)
    threecMaster = ThreeColMid 1 (3/100) (6/8)
    threecSlave = ThreeColMid 1 (3/100) (3/8)


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
      -- spawnOnce "systemctl --user restart quasar-terminal"
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
-- youtube (firefox) and other fullscreen stuff
addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]


------------------------------------------------------------------------------
-- | Customize layouts.
myLayouts = mkToggle1 FULL
  $ avoidStruts  -- no overlap dock, except when fullscreen
  $ spaced 3
  $ mkToggle (MIRROR ?? GRID ?? MAXIMIZE ?? EOT)
  $ decorate
  $ mkToggle (QTILE ?? EOT)
  $ qsrFloatTile ||| borderResize qsrFloat
  where
    spaced d = spacingRaw True (Border d d d d) True (Border d d d d) True
    -- title bar is 'desktop' type window, made transparent by compton
    decorate l = xDecoration shrinkText decorationTheme l

    -- add to myLayouts otherwise type ambiguity
    -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Layout-Combo.html
    -- twopane = combineTwo (TwoPane 0.03 0.5) (tabbed (shrinkText :: DefaultShrinker) defaultThemeWithButtons) (tabbed (shrinkText :: DefaultShrinker) defaultThemeWithButtons)
    -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Layout-ComboP.html

------------------------------------------------------------------------------
-- | Manipulate windows as they are created.
-- @composeOne@ processes first-match from top to bottom
-- Use the `xprop' tool to get the info you need for these matches.
myManageHook = composeOne
  [ className =? "Tilda" -?> doFloat
  -- , className =? "kitty" -?> doFullFloat
  , className =? "quasar-terminal-kitty" -?> do
      w <- ask
      liftX (XS.put
       (UserState { _terminal = Just (Right w) }))
      doF id
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
ifTerminal t f =
  ifWindows (className =? ".terminator-wrapped") (\_ -> t) f

ifNotTermSend k a = ifTerminal (sendKey noModMask k) a

doFullscreen = do
  sendMessage (MT.Toggle FULL)
  raiseFocusedWindow
  -- sendKey noModMask xK_F11

-- TODO restart on terminal exit
terminalToggle :: X ()
terminalToggle = do
  -- system "ps -e | grep .kitty-wrapped | grep -q"
  UserState wterm <- XS.get :: X UserState
  case wterm of
    Nothing -> spawn "systemctl --user restart quasar-terminal-kitty"
    Just (Left w) -> do
      XS.put (UserState 
        { _terminal = Just (Right w) })
      reveal w
      manage w
    Just (Right w) -> do
      XS.put (UserState 
        { _terminal = Just (Left w) })
      hide w
      unmanage w

-- | Keybindings
keyConfig =
  [ ("M-S-q",   shellPrompt promptConfig)
  -- , ("M-S-q",   confirmPrompt promptConfig "exit" (io exitSuccess))
  , ("M-<Return>", spawn "kitty")

  --
  , ("M-k",   launchApp promptConfig "thunar")  -- open dir in thunar
  -- , ("M-k",   appendFilePrompt def "/home/me/NOTES")

  -- fast actions bound to f-keys
  -- , ("<F1>",)   -- used by terminator
  , ("<F2>", sendMessage NextLayout)
  , ("<F3>", sendMessage (MT.Toggle QTILE))
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
  , ("M-m", banish UpperLeft)

  -- cycle windows
  -- , ("M-<Tab>", rotAllUp)  -- TODO handle floating better
  -- , ("M-S-<Tab>", rotAllDown)
  , ("M-<Tab>", windows W.focusUp)
  , ("M-S-<Tab>", windows W.focusDown)
  , ("M-`", windows W.focusDown)
  , ("M-S-`", windows W.focusUp)

  -- move windows across workspaces
  , ("M-d", sendMessage (MT.Toggle MAXIMIZE))
  , ("M-<Up>", sendMessage (MT.Toggle MAXIMIZE))
  , ("M-<Right>", moveTo Next NonEmptyWS)
  , ("M-<Left>", moveTo Prev NonEmptyWS)
  , ("M-S-<Right>", shiftToNext >> nextWS)
  , ("M-S-<Left>", shiftToPrev >> prevWS)

  -- manage windows
  , ("M-w",   kill)

  -- switch layouts
  , ("M-t", sendMessage (MT.Toggle QTILE))
  , ("M-z", sendMessage NextLayout)
  , ("M-f", doFullscreen)
  , ("M-x", sendMessage (MT.Toggle MIRROR))
  , ("M-a", sendMessage Magnifier.Toggle)
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
-- | util functions

raiseFocusedWindow :: X ()
raiseFocusedWindow =
  withDisplay $ \d ->
    withFocused $ \w ->
      io $ raiseWindow d w
  -- raiseWindow :: Display -> Window -> IO ()

raiseFocusedWindowConditional :: (Window -> X Bool) -> X ()
raiseFocusedWindowConditional cond =
  withFocused $ \w -> do
    cond w >>= \case
      False -> return ()
      True -> raiseFocusedWindow

raiseFocusedIfFloating :: X ()
raiseFocusedIfFloating = raiseFocusedWindowConditional (runQuery isFloating)

withQuery :: XM.Query a -> (a -> X ()) -> X ()
withQuery q f = withFocused $ \w -> runQuery q w >>= f

findWindow :: (Window -> X Bool) -> X [Window]
findWindow condition = do
  windows <- gets (W.index . windowset)  -- MonadState XState X
  filterM condition windows


------------------------------------------------------------------------------
-- | server send command
sendCommand :: String -> IO ()
sendCommand = sendCommand' "XMONAD_COMMAND"

sendCommand' :: String -> String -> IO ()
sendCommand' addr s = do
  d   <- openDisplay ""
  rw  <- rootWindow d $ defaultScreen d
  a <- internAtom d addr False
  m <- internAtom d s False
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e rw a 32 m currentTime
    sendEvent d rw False structureNotifyMask e
    sync d False


------------------------------------------------------------------------------
-- | unused currently

-- unused because xautolock wants lock app to not fork
subIndex :: Eq a => [a] -> [a] -> Maybe Int
subIndex substr str = L.findIndex (isPrefixOf substr) (L.tails str)

quasarLock :: X ()
quasarLock = withFocused $ \w -> do
  windowTitle <- runQuery title w  -- title :: Query String
  let yt = subIndex " - YouTube - " windowTitle
  let vlc = subIndex " - VLC media player" windowTitle
  if yt == Nothing && vlc == Nothing
    then spawn "xautolock -locknow"
    else spawn "(pacmd list-sink-inputs | grep 'state: RUNNING') || xautolock -locknow"

quasarSuspend :: X ()
quasarSuspend =
  spawn "(pacmd list-sink-inputs | grep 'state: RUNNING') || systemctl suspend"


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

