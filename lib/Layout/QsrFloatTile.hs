{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  QsrFloatTile
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

module Layout.QsrFloatTile
    ( -- * Usage
      -- $usage
      qsrFloatTile, QsrFloatTile
    ) where

import XMonad
import XMonad.Util.PositionStore
import qualified XMonad.StackSet as S
import XMonad.Layout.WindowArranger
import Control.Monad(when)
import Data.Maybe(isJust)
import Data.List(nub)

import BorderResize
import XMonad.Layout.NoFrillsDecoration

import Data.Ratio ((%))

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import QsrFloatTile
-- > import XMonad.Layout.NoFrillsDecoration
-- > import XMonad.Layout.BorderResize
--
-- Then edit your @layoutHook@ by adding the QsrFloatTile layout.
-- Below is a suggestion which uses the mentioned NoFrillsDecoration and
-- BorderResize:
--
-- > myLayouts = floatingDeco $ borderResize $ qsrFloatTile ||| etc..
-- >               where floatingDeco l = noFrillsDeco shrinkText def l
-- > main = xmonad def { layoutHook = myLayouts }
--
-- See the documentation of "XMonad.Hooks.PositionStoreHooks" on how
-- to add the support hooks.

qsrFloatTile :: QsrFloatTile a
qsrFloatTile = QsrFloatTile (Nothing, [])

data QsrFloatTile a = QsrFloatTile (Maybe Rectangle, [a]) deriving (Show, Read)
instance LayoutClass QsrFloatTile Window where
    description _ = "QsrFloatTile"
    -- doLayout :: layout a -> Rectangle -> Stack a -> X ([(a, Rectangle)], Maybe (layout a)) 
    doLayout (QsrFloatTile _) wr (S.Stack focus up down) = do
        let wy = 100 % 1080
        let dw = 1300 % 1920
        let dh = 800 % 1080

        -- windows started in order: a, b, c
        -- S.Stack w [] [b,a] when looking at c
        -- S.Stack w [c] [a] when looking at b
        -- S.Stack w [b,c] [] when looking at a

        let ldown = fromIntegral (length down)
            k = 1 + ldown + fromIntegral (length up)

            shiftx i k = 100 + if k < 2 then 0 else i * div 420 (k-1)
            rscale i k = S.RationalRect (shiftx i k % 1920) wy dw dh
            rects w i k = (w, scaleRationalRect wr $ rscale i k)

        return $ case k of
            1 -> ( [rects focus 0 1], Nothing )
            _ -> ( [rects focus ldown k] 
                   ++ [rects w i k | (w,i) <- zip up [(ldown+1)..(k-1)]]
                   ++ [rects w i k | (w,i) <- zip down [(ldown-1), (ldown-2)..0]] 
                 , Nothing )
    
    pureMessage (QsrFloatTile (_, paintOrder)) m
        | Just (SetGeometry rect) <- fromMessage m =
            Just $ QsrFloatTile (Just rect, paintOrder)
        | otherwise = Nothing

