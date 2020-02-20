{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  BorderResize
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- This layout modifier will allow to resize windows by dragging their
-- borders with the mouse. However, it only works in layouts or modified
-- layouts that react to the 'SetGeometry' message.
-- "XMonad.Layout.WindowArranger" can be used to create such a setup,
-- but it is probably must useful in a floating layout such as
-- "XMonad.Layout.PositionStoreFloat" with which it has been mainly tested.
-- See the documentation of PositionStoreFloat for a typical usage example.
--
-----------------------------------------------------------------------------

module BorderResize
    ( -- * Usage
      -- $usage
      borderResize
    , BorderResize (..)
    , RectWithBorders, BorderInfo,
    ) where

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.WindowArranger
import XMonad.Util.XUtils
import Control.Monad(when)
import qualified Data.Map as M

import Foreign.C.Types

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import BorderResize
-- > myLayout = borderResize (... layout setup that reacts to SetGeometry ...)
-- > main = xmonad def { layoutHook = myLayout }
--

type BorderBlueprint = (Rectangle, Glyph, BorderType)

data BorderType 
  = RightSideBorder
  | LeftSideBorder
  | TopSideBorder
  | BottomSideBorder
  deriving (Show, Read, Eq)

data BorderInfo = BI 
  { bWin :: Window
  , bRect :: Rectangle
  , bType :: BorderType
  } deriving (Show, Read)

type RectWithBorders = (Rectangle, [BorderInfo])

data BorderResize a = BR (M.Map Window RectWithBorders) deriving (Show, Read)

brBorderSize :: Dimension
brBorderSize = 10

borderResize :: l a -> ModifiedLayout BorderResize l a
borderResize = ModifiedLayout (BR M.empty)

-- TODO remove extraneous stuff from here
instance LayoutModifier BorderResize Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (BR wrsLastTime) _ _ wrs = do
        let correctOrder = map fst wrs
            wrsCurrent = M.fromList wrs
            wrsGone = M.difference wrsLastTime wrsCurrent
            wrsAppeared = M.difference wrsCurrent wrsLastTime
            wrsStillThere = M.intersectionWith testIfUnchanged wrsLastTime wrsCurrent
        handleGone wrsGone
        wrsCreated <- handleAppeared wrsAppeared
        let wrsChanged = handleStillThere wrsStillThere
            wrsThisTime = M.union wrsChanged wrsCreated
        return (compileWrs wrsThisTime correctOrder, Just $ BR wrsThisTime)
        -- What we return is the original wrs with the new border
        -- windows inserted at the correct positions - this way, the core
        -- will restack the borders correctly.
        -- We also return information about our borders, so that we
        -- can handle events that they receive and destroy them when
        -- they are no longer needed.
        where
            testIfUnchanged entry@(rLastTime, _) rCurrent =
                if rLastTime == rCurrent
                    then (Nothing, entry)
                    else (Just rCurrent, entry)

    handleMess (BR wrsLastTime) m
        | Just e <- fromMessage m :: Maybe Event =
            handleResize (createBorderLookupTable wrsLastTime) e >> return Nothing
        | Just _ <- fromMessage m :: Maybe LayoutMessages =
            handleGone wrsLastTime >> return (Just $ BR M.empty)
    handleMess _ _ = return Nothing

compileWrs :: M.Map Window RectWithBorders -> [Window] -> [(Window, Rectangle)]
compileWrs wrsThisTime correctOrder = let wrs = reorder (M.toList wrsThisTime) correctOrder
                                      in concat $ map compileWr wrs

compileWr :: (Window, RectWithBorders) -> [(Window, Rectangle)]
compileWr (w, (r, borderInfos)) =
    let borderWrs = for borderInfos $ \bi -> (bWin bi, bRect bi)
    in borderWrs ++ [(w, r)]

handleGone :: M.Map Window RectWithBorders -> X ()
handleGone wrsGone = mapM_ deleteWindow borderWins
    where
        borderWins = map bWin . concat . map snd . M.elems $ wrsGone

handleAppeared :: M.Map Window Rectangle -> X (M.Map Window RectWithBorders)
handleAppeared wrsAppeared = do
    let wrs = M.toList wrsAppeared
    wrsCreated <- mapM handleSingleAppeared wrs
    return $ M.fromList wrsCreated

handleSingleAppeared :: (Window, Rectangle) -> X (Window, RectWithBorders)
handleSingleAppeared (w, r) = do
    let borderBlueprints = prepareBorders r
    borderInfos <- mapM createBorder borderBlueprints
    return (w, (r, borderInfos))

handleStillThere :: M.Map Window (Maybe Rectangle, RectWithBorders) -> M.Map Window RectWithBorders
handleStillThere wrsStillThere = M.map handleSingleStillThere wrsStillThere

handleSingleStillThere :: (Maybe Rectangle, RectWithBorders) -> RectWithBorders
handleSingleStillThere (Nothing, entry) = entry
handleSingleStillThere (Just rCurrent, (_, borderInfos)) = (rCurrent, updatedBorderInfos)
    where
        changedBorderBlueprints = prepareBorders rCurrent
        updatedBorderInfos = map updateBorderInfo . zip borderInfos $ changedBorderBlueprints
          -- assuming that the four borders are always in the same order

updateBorderInfo :: (BorderInfo, BorderBlueprint) -> BorderInfo
updateBorderInfo (borderInfo, (r, _, _)) = borderInfo { bRect = r }

createBorderLookupTable :: M.Map Window RectWithBorders -> [(Window, (BorderType, Window, Rectangle))]
createBorderLookupTable wrsLastTime = concat $ map processSingleEntry $ M.toList wrsLastTime
    where
        processSingleEntry :: (Window, RectWithBorders) -> [(Window, (BorderType, Window, Rectangle))]
        processSingleEntry (w, (r, borderInfos)) = for borderInfos $ \bi -> (bWin bi, (bType bi, w, r))

prepareBorders :: Rectangle -> [BorderBlueprint]
prepareBorders (Rectangle x y wh ht) =
    [((Rectangle (x + fi wh - fi brBorderSize) y brBorderSize ht), xC_right_side , RightSideBorder),
     ((Rectangle x y brBorderSize ht)                            , xC_left_side  , LeftSideBorder),
     ((Rectangle x y wh brBorderSize)                            , xC_top_side   , TopSideBorder),
     ((Rectangle x (y + fi ht - fi brBorderSize) wh brBorderSize), xC_bottom_side, BottomSideBorder)
    ]


-- 
handleResize :: [(Window, (BorderType, Window, Rectangle))] -> Event -> X ()
handleResize borders ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress, Just (_, w, _) <- lookup ew borders =
    whenX (isClient w) $ withDisplay $ \d -> do
        wa <- io $ getWindowAttributes d w
        sh <- io $ getWMNormalHints d w
        (_, _, _, _, _, ix, iy, _) <- io $ queryPointer d w
        let [pos_x, pos_y, width, height] = map (fi . ($ wa)) [wa_x, wa_y, wa_width, wa_height]
            west  = findPos ix width
            north = findPos iy height
            (cx, fx, gx) = mkSel west  width  pos_x
            (cy, fy, gy) = mkSel north height pos_y
        io $ warpPointer d none w 0 0 0 0 cx cy
        mouseDrag (\ex ey -> do let (nw,nh) = applySizeHintsContents sh (gx ex, gy ey)
                                let rect = Rectangle (fx nw) (fy nh) nw nh
                                sendMessage (SetGeometry rect))
                  (focus w)
        where
        edge :: Rational
        edge = 0
        
        findPos :: CInt -> Position -> Maybe Bool
        findPos m s = if p < 0.5 - edge/2
            then Just True
            else if p < 0.5 + edge/2
                then Nothing
                else Just False
            where p = fi m / fi s

        mkSel :: Maybe Bool -> Position -> Position -> (Position, Dimension -> Position, Position -> Dimension)
        mkSel b k p = case b of
            Just True ->  (0, (fi k + fi p -).fi, (fi k + fi p -).fi)
            Nothing ->    (k `div` 2, const p, const $ fi k)
            Just False -> (k, const p, subtract (fi p) . fi)


-- handleResize :: [(Window, (BorderType, Window, Rectangle))] -> Event -> X ()
-- handleResize borders ButtonEvent { ev_window = ew, ev_event_type = et }
--     | et == buttonPress, Just edge <- lookup ew borders =
--     case edge of
--         (RightSideBorder, hostWin, (Rectangle hx hy _ hht)) ->
--             mouseDrag (\x _ -> do
--                             let nwh = max 1 $ fi (x - hx)
--                                 rect = Rectangle hx hy nwh hht
--                             focus hostWin
--                             when (x - hx > 0) $ sendMessage (SetGeometry rect)) (focus hostWin)
--         (LeftSideBorder, hostWin, (Rectangle hx hy hwh hht)) ->
--             mouseDrag (\x _ -> do
--                             let nx = max 0 $ min (hx + fi hwh) $ x
--                                 nwh = max 1 $ hwh + fi (hx - x)
--                                 rect = Rectangle nx hy nwh hht
--                             focus hostWin
--                             when (x < hx + fi hwh) $ sendMessage (SetGeometry rect)) (focus hostWin)
--         (TopSideBorder, hostWin, (Rectangle hx hy hwh hht)) ->
--             mouseDrag (\_ y -> do
--                             let ny = max 0 $ min (hy + fi hht) $ y
--                                 nht = max 1 $ hht + fi (hy - y)
--                                 rect = Rectangle hx ny hwh nht
--                             focus hostWin
--                             when (y < hy + fi hht) $ sendMessage (SetGeometry rect)) (focus hostWin)
--         (BottomSideBorder, hostWin, (Rectangle hx hy hwh _)) ->
--             mouseDrag (\_ y -> do
--                             let nht = max 1 $ fi (y - hy)
--                                 rect = Rectangle hx hy hwh nht
--                             focus hostWin
--                             when (y - hy > 0) $ sendMessage (SetGeometry rect)) (focus hostWin)
handleResize _ _ = return ()

createBorder :: BorderBlueprint -> X (BorderInfo)
createBorder (borderRect, borderCursor, borderType) = do
    borderWin <- createInputWindow borderCursor borderRect
    return BI { bWin = borderWin, bRect = borderRect, bType = borderType }

createInputWindow :: Glyph -> Rectangle -> X Window
createInputWindow cursorGlyph r = withDisplay $ \d -> do
    win <- mkInputWindow d r
    io $ selectInput d win (exposureMask .|. buttonPressMask)
    cursor <- io $ createFontCursor d cursorGlyph
    io $ defineCursor d win cursor
    io $ freeCursor d cursor
    showWindow win
    return win

mkInputWindow :: Display -> Rectangle -> X Window
mkInputWindow d (Rectangle x y w h) = do
  rw <- asks theRoot
  let screen   = defaultScreenOfDisplay d
      visual   = defaultVisualOfScreen screen
      attrmask = cWOverrideRedirect
  io $ allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes True
           createWindow d rw x y w h 0 0 inputOnly visual attrmask attributes

for :: [a] -> (a -> b) -> [b]
for = flip map

reorder :: (Eq a) => [(a, b)] -> [a] -> [(a, b)]
reorder wrs order =
    let ordered = concat $ map (pickElem wrs) order
        rest = filter (\(w, _) -> not (w `elem` order)) wrs
    in ordered ++ rest
    where
        pickElem list e = case (lookup e list) of
                                Just result -> [(e, result)]
                                Nothing -> []
