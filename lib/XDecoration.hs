{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ButtonDecoration
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- A decoration that includes small buttons on both ends which invoke
-- various actions when clicked on: Show a window menu (see
-- "XMonad.Actions.WindowMenu"), minimize, maximize or close the window.
--
-- Note: For maximizing and minimizing to actually work, you will need
-- to integrate "XMonad.Layout.Maximize" and "XMonad.Layout.Minimize" into your
-- setup.  See the documentation of those modules for more information.
--
-----------------------------------------------------------------------------

module XDecoration
    ( -- * Usage:
      -- $usage
      xDecoration,
      XDecoration,
      decorationTheme,
    ) where

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Util.Font

import XMonad.Layout.DraggingVisualizer
import qualified XMonad.StackSet as S
import Control.Monad
import Foreign.C.Types(CInt)

-- import XMonad.Layout.ImageButtonDecoration
-- import XMonad.Layout.ButtonDecoration
-- import XMonad.Layout.TabBarDecoration

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.DecorationAddons
-- > import XMonad.Layout.ButtonDecoration
--
-- Then edit your @layoutHook@ by adding the ButtonDecoration to
-- your layout:
--
-- > myL = buttonDeco shrinkText defaultThemeWithButtons (layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--

data XDecoration a = XDecoration Bool deriving (Show, Read)

instance Eq a => DecorationStyle XDecoration a where
    describeDeco _ = "XDecoration"
    
    -- decorationEventHook :: ds a -> DecorationState -> Event -> X ()
    -- The decoration event hook
    
    decorationCatchClicksHook xdeco win dFL dFR = 
      titleBarButtonHandler win dFL dFR
    
    -- decorationWhileDraggingHook 
    --   :: ds a -> CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X () 
    -- This hook is called while a window is dragged using the decoration.
    -- The hook can be overwritten if a different way of handling the dragging is required.

    -- decorationAfterDraggingHook _ (win, _) decoWin = do
    --   focus win
    --   handleScreenCrossing win decoWin
    --   return ()
    decorationWhileDraggingHook _ ex ey (mainw, r) x y = 
      handleTiledDraggingInProgress ex ey (mainw, r) x y
    
    decorationAfterDraggingHook _ (mainw, _) decoWin = do 
      focus mainw
      hasCrossed <- handleScreenCrossing mainw decoWin
      unless hasCrossed $ sendMessage DraggingStopped
      performWindowSwitching mainw

    -- pureDecoration :: typeof decorate 
    -- pure version of decorate

    -- decorate :: ds a -> Dimension -> Dimension -> Rectangle -> Stack a 
    --          -> [(a, Rectangle)] -> (a, Rectangle) -> X (Maybe Rectangle)
      -- Given the theme's decoration width and height, the screen rectangle, 
      -- the windows stack, the list of windows and rectangles returned by the 
      -- underlying layout and window to be decorated, tupled with its rectangle, 
      -- produce a Just Rectangle or Nothing if the window is not to be decorated.

--
handleTiledDraggingInProgress :: CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
handleTiledDraggingInProgress ex ey (mainw, r) x y = do
  let rect = Rectangle (x - (fi ex - rect_x r))
                        (y - (fi ey - rect_y r))
                        (rect_width  r)
                        (rect_height r)
  sendMessage $ DraggingWindow mainw rect

performWindowSwitching :: Window -> X ()
performWindowSwitching win =
  withDisplay $ \d -> do
    root <- asks theRoot
    (_, _, selWin, _, _, _, _, _) <- io $ queryPointer d root
    ws <- gets windowset
    let allWindows = S.index ws
    -- do a little double check to be sure
    if (win `elem` allWindows) && (selWin `elem` allWindows)
      then do
        let allWindowsSwitched = map (switchEntries win selWin) allWindows
        let (ls, t:rs) = break (win ==) allWindowsSwitched
        let newStack = S.Stack t (reverse ls) rs
        windows $ S.modify' $ \_ -> newStack
      else return ()
  where
    switchEntries a b x
      | x == a    = b
      | x == b    = a
      | otherwise = x


------------------------------------------------------------------------------
xDecoration :: (Eq a, Shrinker s) 
            => s 
            -> Theme
            -> l a 
            -> ModifiedLayout (Decoration XDecoration s) l a
xDecoration s c = decoration s c $ XDecoration True

decorationTheme :: Theme
decorationTheme = Theme 
  { activeColor         = "#ffffff"
  , inactiveColor       = "#4c4c4c"
  , urgentColor         = "#FFFF00"
  , activeBorderColor   = "#ffffff"
  , inactiveBorderColor = "#4c4c4c"
  , urgentBorderColor   = "#FFFF00"
  -- , activeBorderWidth   = 1
  -- , inactiveBorderWidth = 1
  -- , urgentBorderWidth   = 1
  , activeTextColor     = "#404040"
  , inactiveTextColor   = "#BFBFBF"
  , urgentTextColor     = "#FF0000"
  , fontName            = "xft:bitstream vera sans mono:bold:size=9:antialias=true"
  , decoWidth           = 200
  , decoHeight          = 25
  , windowTitleAddons   = 
    [ (" (M)", AlignLeft)
    , ("_"   , AlignRightOffset 48)
    , ("[]"  , AlignRightOffset 25)
    , ("X"   , AlignRightOffset 10)
    ]
  , windowTitleIcons    = []
  }


