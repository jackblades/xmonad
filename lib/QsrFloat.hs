{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  QsrFloat
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

module QsrFloat
    ( -- * Usage
      -- $usage
      qsrFloat, QsrFloat
    ) where

import XMonad
import PositionStore
import qualified XMonad.StackSet as S
import XMonad.Layout.WindowArranger
import Control.Monad(when,forM_)
import Data.Maybe(isJust)
import Data.List(nub)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import QsrFloat
-- > import XMonad.Layout.NoFrillsDecoration
-- > import XMonad.Layout.BorderResize
--
-- Then edit your @layoutHook@ by adding the QsrFloat layout.
-- Below is a suggestion which uses the mentioned NoFrillsDecoration and
-- BorderResize:
--
-- > myLayouts = floatingDeco $ borderResize $ qsrFloat ||| etc..
-- >               where floatingDeco l = noFrillsDeco shrinkText def l
-- > main = xmonad def { layoutHook = myLayouts }
--
-- See the documentation of "XMonad.Hooks.PositionStoreHooks" on how
-- to add the support hooks.

qsrFloat :: QsrFloat a
qsrFloat = QsrFloat (Nothing, [])

data QsrFloat a = QsrFloat (Maybe Rectangle, [a]) deriving (Show, Read)
instance LayoutClass QsrFloat Window where
    description _ = "QsrFloat"
    -- doLayout :: layout a -> Rectangle -> Stack a -> X ([(a, Rectangle)], Maybe (layout a)) 
    doLayout (QsrFloat (maybeChange, paintOrder)) sr@(Rectangle _ _ w h) (S.Stack focus up down) = do
        let fi = fromIntegral
        let scalex x = round (x * fi w / 1920)
        let scaley y = round (y * fi h / 1080)

        let xdef = scalex 75
        let xshift = scalex 500
        let ydef = scaley 50
        let yshift = scalex 130
        
        let wdef = scalex 1300
        let hdef = scaley 800

        -- windows started in order: a, b, c
        -- S.Stack w [] [b,a] when looking at c
        -- S.Stack w [c] [a] when looking at b
        -- S.Stack w [b,c] [] when looking at a

        posStore <- getPosStore

        let ldown = fromIntegral (length down)
            k = 1 + ldown + fromIntegral (length up)

            shifty i k = ydef + if k < 2 then 0 else i * div yshift (k-1)
            shiftx i k = xdef + if k < 2 then 0 else i * div xshift (k-1)
            rects w i k = Rectangle (shiftx i k) (shifty i k) wdef hdef

            -- 
            pSQ posStore sr' w' i k = 
                case (posStoreQuery posStore w' sr') of
                Just rect   -> rect
                Nothing     -> rects w' i k
    
            wrs = [(w, pSQ posStore sr w i k) | (w,i) <- zip up [(ldown+1)..(k-1)]]
               ++ [(w, pSQ posStore sr w i k) | (w,i) <- zip down [(ldown-1), (ldown-2)..0]]
        
            focused = 
                case maybeChange of
                Nothing -> (focus, pSQ posStore sr focus ldown k)
                Just changedRect -> (focus, changedRect)
        
            wrs' = focused : wrs
            paintOrder' = nub (focus : paintOrder)
        
        when (isJust maybeChange) $
            updatePositionStore focused sr
        
        return (reorder wrs' paintOrder', Just $ QsrFloat (Nothing, paintOrder'))

    pureMessage (QsrFloat (_, paintOrder)) m
        | Just (SetGeometry rect) <- fromMessage m =
            Just $ QsrFloat (Just rect, paintOrder)
        | otherwise = Nothing

--
updatePositionStore :: (Window, Rectangle) -> Rectangle -> X ()
updatePositionStore (w, rect) sr =
    modifyPosStore $ \ps -> posStoreInsert ps w rect sr

reorder :: (Eq a) => [(a, b)] -> [a] -> [(a, b)]
reorder wrs order =
    let ordered = concat $ map (pickElem wrs) order
        rest = filter (\(w, _) -> not (w `elem` order)) wrs
    in ordered ++ rest
    where
        pickElem list e = case (lookup e list) of
                                Just result -> [(e, result)]
                                Nothing -> []
