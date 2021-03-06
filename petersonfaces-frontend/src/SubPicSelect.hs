{-|
Module: SubPicSelect
Description: A widget for making muliple selections within an image
Copyright: (c) Greg Hale, 2016
License: BSD3
Maintainer: imalsogreg@gmail.com
Stability: experimental
Portability: GHCJS
-}

{-# language CPP #-}
{-# language FlexibleContexts #-}
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}

module SubPicSelect where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad            (join, liftM2)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Bool
import           Data.Default             (Default, def)
import           Data.List                ((\\), foldl')
import           Data.Maybe               (isJust)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import           Reflex.Dom               hiding (restore)
import           GHCJS.DOM.EventM         (EventM)
import           GHCJS.DOM.EventM         (on, event, stopPropagation, preventDefault)
import           GHCJS.DOM.WheelEvent     as WheelEvent
import qualified GHCJS.DOM.Element        as E

import Common
import ScaledImage
import FaceFeatures

data SubPicSelectConfig t = SubPicSelectConfig
  { spsc_imgSrc           :: T.Text
  , spsc_width            :: Int
  , spsc_initialAspect    :: Double
  , spsc_setAspect        :: Event t Double
  , spsc_initialBoxes     :: Map Int BoundingBox
  , spsc_setBoxes         :: Event t (Map Int (Maybe BoundingBox))
  , spsc_initialSelection :: Maybe Int
  , spsc_setSelection     :: Event t (Maybe Int)
  }

instance Reflex t => Default (SubPicSelectConfig t) where
  def = SubPicSelectConfig "" 400 1 never mempty never Nothing never

data SubPicSelect t = SubPicSelect
  { sps_boxes :: Dynamic t (Map Int BoundingBox)
  , sps_selection :: Dynamic t (Maybe (Int,BoundingBox))
  , sps_img   :: ScaledImage t
  }


subPicSelect :: forall t m.MonadWidget t m => SubPicSelectConfig t -> m (SubPicSelect t)
subPicSelect SubPicSelectConfig{..} = do
  rec img <- scaledImage def { sicInitialSource   = spsc_imgSrc
                             , sicTopLevelScale   = topScale
                             , sicInitialBounding = Nothing
                             }
      let topScale = ffor (siNaturalSize img) $ \(natWid,_) ->
            fI spsc_width / fI natWid

      wheels <- fmap (/200) <$> wrapDomEvent (_element_raw (siEl img))
        (`on` E.wheel) (event >>= getDeltaY)

      aspect <- holdDyn spsc_initialAspect spsc_setAspect
      frac <- foldDyn ($) 0.1 (ffor wheels $ \w f -> min 1 (f * (1+w)))
      pos  <- holdDyn Nothing $
        leftmost [ Just <$>   domEvent Mousemove  (siEl img)
                 , Nothing <$ domEvent Mouseleave (siEl img)
                 ]

      let bb = mkBounding <$> aspect <*> siNaturalSize img <*> frac <*> pos
          bb' = zipDynWith (\b s -> bool Nothing b (s == Nothing)) bb selection
      display bb'
      (allBoxes, selection) <- listSelfDeleting mempty (boxInserts) spsc_setSelection
        (selectionMarker (imageToScreenSpace img) selection (constDyn Nothing {- TODO:  from highlight, drop -}))
      --bbVis <- holdDyn (BoundingBox (Coord 1 1) (Coord 10 10)) (fmapMaybe id (updated bb))
      let boxInserts = fmapMaybe id $ tagPromptlyDyn bb (domEvent Dblclick $ siEl img)


  -- highlight <- holdDyn Nothing never

  let getBox' :: Maybe Int -> Map Int BoundingBox -> Maybe BoundingBox
      getBox' i m = i >>= flip Map.lookup m

      selectionBox :: Dynamic t (Maybe BoundingBox) = zipDynWith getBox' selection allBoxes

  let surroundAttrs = "class" =: "surround-mask" <>
            "style" =: "background-color: rgba(0,0,0,0.10); pointer-events: none;"
      surroundBBs = (\(wid',hei') searchBB selBB hlBB ->
                       let innerBB = foldl' (<|>) Nothing [selBB, hlBB, searchBB]
                       in case innerBB of
                         Nothing -> [BoundingBox (Coord 0 0) (Coord (fI wid') (fI hei'))]
                         Just (BoundingBox (Coord x0 y0) (Coord x1 y1))->
                           let (wid,hei) = (fI wid', fI hei') in
                           [ BoundingBox (Coord 0 0) (Coord wid y0)
                           , BoundingBox (Coord 0 y0) (Coord x0 y1), BoundingBox (Coord x1 y0) (Coord wid y1)
                           , BoundingBox (Coord 0 y1) (Coord wid hei)])
        <$> (siNaturalSize img) <*> bb <*> selectionBox <*> constDyn Nothing -- TODO: last arg from highlightBox. drop it
  dyn (ffor bb $ \case
    Nothing -> return ()
    Just bb' -> divImgSpace (imageToScreenSpace img)
      (constDyn bb') (constDyn $ "class" =: "select-roi"
                      <> "style" =: ("pointer-events: none; border: 0px solid black; " <>
                                     "box-shadow: 0px 0px 10px rgba(0,0,0,0.5);")) (return ()))
  simpleList surroundBBs (\sbb -> divImgSpace (imageToScreenSpace img) sbb (constDyn surroundAttrs) (return ()))
  display selection
  return (SubPicSelect allBoxes (liftA2 (,) <$> selection <*> selectionBox) img)


selectionMarker :: forall t m. MonadWidget t m
                => Dynamic t ((Double,Double) -> (Double,Double))
                -> Dynamic t (Maybe Int)
                -> Dynamic t (Maybe Int)
                -> Int
                -> BoundingBox
                -> Event t BoundingBox
                -> m (BoundingBox, Event t DeselectMe, Event t DeleteMe, Event t SelectMe)
selectionMarker toScreen sel hil k bb _ = do
  let isSelected = (== Just k) <$> sel -- TODO demux
  let divAttrs = ffor isSelected $ bool ("style" =: "opacity: 0.5; border: 1px solid black; pointer-events: none;")
                                         ("style" =: "opacity: 1;   border: 1px solid black; pointer-events: none;")
  (d,(toggleSel, del)) <- divImgSpace' toScreen (constDyn bb) divAttrs $ do
    togl <- fst <$> elAttr' "div" ("class" =: "ok-button" <>
                                   "style" =: ("position:absolute; bottom: 0px; width:10px; height: 10px; " <>
                                               "background-color: green; pointer-events: auto;")) (return ())
    del  <- fst <$> elAttr' "div" ("class" =: "ok-button" <>
                                   "style" =: ("position:absolute; bottom: 0px; right: 0px; " <>
                                               "width:10px; height: 10px; background-color: red; pointer-events: auto;"))
      (return ())
    return (domEvent Click togl, DeleteMe <$ domEvent Click del)
  let deselects = DeselectMe <$ gate (current isSelected) toggleSel
      selects   = SelectMe   <$ gate (fmap not $ current isSelected) toggleSel
  return (bb, deselects, del, selects)
  -- return ((FaceAttributes bb (constDyn mempty)), deselects, del, selects)




selectSizeAtFrac :: Double -> (Int,Int) -> Double -> (Double,Double)
selectSizeAtFrac selectionAspectRatio (natWid,natHei) fractionFull =
  let imgAspect    = fI natWid / fI natHei
      widthLimited = selectionAspectRatio < imgAspect
      wid       = bool (fractionFull * fI natWid) (hei * selectionAspectRatio) widthLimited
      hei       = bool (wid / selectionAspectRatio) (fractionFull * fI natHei) widthLimited
  in  (wid, hei)

boundFocusCoord :: Double -> (Int,Int) -> Double -> (Int,Int) -> (Double, Double)
boundFocusCoord selectionAspectRatio natGeom@(natWid,natHei) fracFull (x,y) =
  let (selWid,selHei) = selectSizeAtFrac selectionAspectRatio natGeom fracFull
      toRange rLow rHigh = max rLow . min rHigh
      boundedX = toRange (selWid/2) (fI natWid - selWid/2) (fI x)
      boundedY = toRange (selHei/2) (fI natHei - selHei/2) (fI y)
  in  (boundedX, boundedY)

-- mkBounding :: Double -> (Int,Int) -> Double -> Maybe (Int,Int) -> Maybe Int -> Maybe BoundingBox
mkBounding :: Double -> (Int,Int) -> Double -> Maybe (Int,Int) -> Maybe BoundingBox
mkBounding _               _       _           Nothing  = Nothing
--mkBounding _               _       _           _        (Just _) = Nothing
mkBounding selectionAspect natGeom scaleFactor (Just f) = Just $
  let (wid,hei) = selectSizeAtFrac selectionAspect natGeom scaleFactor
      (x,y)     = boundFocusCoord  selectionAspect natGeom scaleFactor f
  in  BoundingBox (Coord (x - wid/2) (y - hei/2))
                  (Coord (x + wid/2) (y + hei/2))
