{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  QsrTile
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- A floating layout which has been designed with a dual-head setup
-- in mind. It makes use of "XMonad.Util.PositionStore" as well as
-- "XMonad.Hooks.PositionStoreHooks" . Since there is currently no way
-- to move or resize windows with the keyboard alone in this layout,
-- it is adviced to use it in combination with a decoration such as
-- "XMonad.Layout.NoFrillsDecoration" (to move windows) and the
-- layout modifier "XMonad.Layout.BorderResize" (to resize windows).
--
-----------------------------------------------------------------------------

module QsrTile
    ( -- * Usage
      -- $usage
      qsrGTile, qsr3Tile, QsrTile
    ) where

import XMonad
import XMonad.Util.PositionStore
import qualified XMonad.StackSet as S
import XMonad.Layout.WindowArranger
import Control.Monad(when)
import Data.Maybe(isJust)
import Data.List(nub)

import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import QsrTile
-- > import XMonad.Layout.NoFrillsDecoration
-- > import XMonad.Layout.BorderResize
--
-- Then edit your @layoutHook@ by adding the QsrTile layout.
-- Below is a suggestion which uses the mentioned NoFrillsDecoration and
-- BorderResize:
--
-- > myLayouts = floatingDeco $ borderResize $ qsrTile ||| etc..
-- >               where floatingDeco l = noFrillsDeco shrinkText def l
-- > main = xmonad def { layoutHook = myLayouts }
--
-- See the documentation of "XMonad.Hooks.PositionStoreHooks" on how
-- to add the support hooks.


-- TODO basically is floattile right now, could be used to implement tiling

qsrGTile :: QsrTile a
qsrGTile = QsrTile TTGrid (Nothing, [])

qsr3Tile :: QsrTile a
qsr3Tile = QsrTile TTThreeCol (Nothing, [])

data TileType = TTGrid | TTThreeCol deriving (Show, Read)

data QsrTile a = QsrTile TileType (Maybe Rectangle, [a]) deriving (Show, Read)
instance LayoutClass QsrTile Window where
    description _ = "QsrTile"
    -- doLayout :: layout a -> Rectangle -> Stack a -> X ([(a, Rectangle)], Maybe (layout a)) 
    doLayout (QsrTile tileType _) (Rectangle _ _ w h) st = case st of 
        -- 1 window
        S.Stack focus [] [] -> return $ ( [rects focus 0] , Nothing )
        -- 2 windows
        S.Stack focus [b] [] -> return $ ( [rects focus 0, rects b 1] , Nothing )
        S.Stack focus [] [a] -> return $ ( [rects focus 1, rects a 0] , Nothing )
        --
        _ -> case tileType of
            TTGrid -> do
                (wrs, _) <- doLayout Grid rectp st 
                return (wrs, Nothing)
            TTThreeCol -> do
                (wrs, _) <- doLayout (ThreeColMid 1 (3/100) (1/2)) rectp st
                return (wrs, Nothing)

        where fi = fromIntegral
              scalex x = round (x * fi w / 1920)
              scaley y = round (y * fi h / 1080)

              rects w i = (w, Rectangle (scalex 50 + scalex 910 * i) (scaley 75) (scalex 900) (scaley 900))
              rectp = Rectangle (scalex 50) (scaley 50) (scalex 1810) (scaley 1000)
    
    pureMessage (QsrTile tileType (_, paintOrder)) m
        | Just (SetGeometry rect) <- fromMessage m =
            Just $ QsrTile tileType (Just rect, paintOrder)
        | otherwise = Nothing

