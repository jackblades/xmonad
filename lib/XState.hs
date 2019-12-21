
module XState
    ( -- * Usage
      -- $usage
      quasarTerminalKittyHook,
      terminalToggle,      
    ) where

--
import XMonad
import qualified XMonad.Util.ExtensibleState as XS

--
data UserState = UserState 
  { _terminal :: Maybe (Either Window Window)
  } deriving (Read, Show, Typeable)

instance ExtensionClass UserState where
  initialValue = UserState { _terminal = Nothing }
  extensionType = PersistentExtension  -- survive mod-q

--
quasarTerminalKittyHook :: ManageHook
quasarTerminalKittyHook = do
    w <- ask
    liftX (XS.put
     (UserState { _terminal = Just (Right w) }))
    doF id

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

