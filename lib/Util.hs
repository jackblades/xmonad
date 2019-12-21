{-# LANGUAGE LambdaCase #-}


module Util where

import XMonad 
import XMonad.Hooks.FadeWindows (isFloating)
import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances as MTI
import XMonad.Actions.WindowGo (ifWindows)
import XMonad.Util.Paste (sendKey)
import qualified XMonad.Core as XM (Query)
import qualified XMonad.StackSet as W

import Control.Monad (when, join, filterM)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList)
import qualified Data.List as L (find, findIndex, tails)

------------------------------------------------------------------------------
-- | util functions

ifTerminal t f =
  ifWindows (className =? ".terminator-wrapped") (\_ -> t) f

ifNotTermSend k a = ifTerminal (sendKey noModMask k) a

doFullscreen :: X ()
doFullscreen = do
  sendMessage (MT.Toggle FULL)
  raiseFocusedWindow

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

