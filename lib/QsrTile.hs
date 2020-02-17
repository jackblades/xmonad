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
      rectScale1, rectScale40,
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

import Data.Ratio ((%))

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


rectScale1 :: S.RationalRect
rectScale1 = S.RationalRect (0 % 1) (0 % 1) (1 % 1) (1 % 1)

rectScale2 :: S.RationalRect
rectScale2 = S.RationalRect (0 % 1) (0 % 1) (3 % 4) (1 % 1)

rectScale40 :: S.RationalRect
rectScale40 = S.RationalRect (1 % 40) (1 % 40) (38 % 40) (36 % 40)

--

qsrGTile :: S.RationalRect -> QsrTile a
qsrGTile rscale = QsrTile rscale TTGrid (Nothing, [])

qsr3Tile :: S.RationalRect -> QsrTile a
qsr3Tile rscale = QsrTile rscale TTThreeCol (Nothing, [])

data TileType = TTGrid | TTThreeCol deriving (Show, Read)

data QsrTile a = QsrTile S.RationalRect TileType (Maybe Rectangle, [a]) deriving (Show, Read)
instance LayoutClass QsrTile Window where
    description _ = "QsrTile"
    -- doLayout :: layout a -> Rectangle -> Stack a -> X ([(a, Rectangle)], Maybe (layout a)) 
    doLayout (QsrTile rscale tileType _) wr0 st = do
        let wr = scaleRationalRect wr0 rscale
        
        case tileType of
          TTGrid -> gridLayout wr st
          TTThreeCol -> do
              (wrs, _) <- doLayout (ThreeColMid 1 (3/100) (1/2)) wr st
              return (wrs, Nothing)
    
    pureMessage (QsrTile r tileType (_, paintOrder)) m
        | Just (SetGeometry rect) <- fromMessage m =
            Just $ QsrTile r tileType (Just rect, paintOrder)
        | otherwise = Nothing

--
gridLayout wr (S.Stack focus up down) = do
    let ldown = fromIntegral (length down)
    let k = 1 + ldown + fromIntegral (length up)

    let cols = ceiling (sqrt (fromIntegral k))
    let rows = ceiling (fromIntegral k / fromIntegral cols)
    let rem = Prelude.rem k rows

    let rect w' i = (w', scaleRationalRect wr $ S.RationalRect wx wy dw dh) where
          (x, y) = divMod i rows
          rowsW = if x == cols - 1 && rem /= 0 then rem else rows
          (wx, wy) = (x % cols, y % rowsW)
          dw = 1 % cols
          dh = 1 % rowsW

    -- windows started in order: a, b, c
    -- S.Stack w [] [b,a] when looking at c
    -- S.Stack w [c] [a]  when looking at b
    -- S.Stack w [b,c] [] when looking at a

    return $ case k of
        1 -> ( [(focus, scaleRationalRect wr rectScale2)], Nothing )
        _ -> ( [rect focus ldown] 
                ++ [rect w i | (w,i) <- zip up [(ldown+1)..(k-1)]]
                ++ [rect w i | (w,i) <- zip down [(ldown-1), (ldown-2)..0]] 
                , Nothing )







