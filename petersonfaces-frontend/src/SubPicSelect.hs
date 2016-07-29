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
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}

module SubPicSelect where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad            (liftM2)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Bool
import           Data.Default             (Default, def)
import           Data.List                ((\\))
import           Data.Maybe               (isJust)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import           Reflex.Dom               hiding (restore)
import           GHCJS.DOM.EventM         (EventM)
#ifdef ghcjs_HOST_OS
import GHCJS.DOM.HTMLCanvasElement        (getContext, castToHTMLCanvasElement)
import GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D, save, restore, getImageData)
import           GHCJS.DOM.Types          (ImageData, Nullable, nullableToMaybe)
import           GHCJS.Marshal            (fromJSValUnchecked, toJSVal)
import           GHCJS.DOM.Element        (getClientTop, getClientLeft)
import           GHCJS.DOM.ClientRect     (getTop, getLeft)
#endif
import           GHCJS.DOM.Types          (IsGObject, HTMLCanvasElement, HTMLImageElement)
import           GHCJS.DOM.CanvasRenderingContext2D as Context2D
import qualified GHCJS.DOM.HTMLImageElement as ImageElement
import           GHCJS.DOM.EventM         (on, event, stopPropagation, preventDefault)
import qualified GHCJS.DOM.ClientRect     as CR
import           GHCJS.DOM.Element        (getClientTop, getClientLeft)
import           GHCJS.DOM.MouseEvent     (getClientX, getClientY)
import qualified GHCJS.DOM.Types          as T
import           GHCJS.DOM.WheelEvent     as WheelEvent
import qualified GHCJS.DOM.Element        as E

import ScaledImage

data SubPicSelectConfig t = SubPicSelectConfig
  { spsc_imgSrc           :: String
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
  }


subPicSelect :: MonadWidget t m => SubPicSelectConfig t -> m (SubPicSelect t)
subPicSelect SubPicSelectConfig{..} = do
  rec img <- scaledImage def { sicInitialSource   = spsc_imgSrc
                             , sicTopLevelScale   = topScale
                             , sicInitialBounding = Nothing
                             }
      topScale <- forDyn (siNaturalSize img) $ \(natWid,_) ->
        fI spsc_width / fI natWid

  wheels <- fmap (/200) <$> wrapDomEvent (_el_element (siEl img))
                                         (`on` E.wheel) (event >>= getDeltaY)

  aspect <- holdDyn spsc_initialAspect spsc_setAspect
  frac <- foldDyn ($) 0.1 (ffor wheels $ \w f -> min 1 (f * (1+w)))
  pos  <- holdDyn Nothing $
    leftmost [ Just <$>   domEvent Mousemove  (siEl img)
             , Nothing <$ domEvent Mouseleave (siEl img)
             ]
  bb <- mkBounding `mapDyn` aspect `apDyn` siNaturalSize img `apDyn` frac `apDyn` pos
  bb <- holdDyn (BoundingBox (Coord 1 1) (Coord 10 10)) (fmapMaybe id (updated bb))
  divImgSpace (widgetToScreenSpace img) bb (constDyn $ "style" =: "border: 1px solid black;") (return ())
  return undefined


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

mkBounding :: Double -> (Int,Int) -> Double -> Maybe (Int,Int) -> Maybe BoundingBox
mkBounding _               _       _           Nothing  = Nothing
mkBounding selectionAspect natGeom scaleFactor (Just f) = Just $
  let (wid,hei) = selectSizeAtFrac selectionAspect natGeom scaleFactor
      (x,y)     = boundFocusCoord  selectionAspect natGeom scaleFactor f
  in  BoundingBox (Coord (x - wid/2) (y - hei/2))
                  (Coord (x + wid/2) (y + hei/2))

-- selectionZoomLimits :: AspectRatio -> Zoom -> (Int,Int) -> (Double,Double)
-- selectionZoomLimits topScale selectionAspect imgGeom(imgWid,imgHei) =
--   let minZoomPix = 3
--       minZoomWid = topScale * max selectionAspect (1/selectionAspect)
--       natAspect  = fI imgWid / fI imgHei
--       maxZoom    = bool 

#ifndef ghcjs_HOST_OS
fromJSValUnchecked = error ""
toJSVal = error ""


getContext :: MonadIO m => HTMLCanvasElement -> String -> m CanvasRenderingContext2D
getContext = error "getContext only available in ghcjs"

getImageData :: CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> IO (Maybe ImageData)
getImageData = error "getImageData only available in ghcjs"

castToHTMLCanvasElement :: IsGObject obj => obj -> HTMLCanvasElement
castToHTMLCanvasElement = error "castToHTMLCanvasElement only available in ghcjs"

save :: MonadIO m => CanvasRenderingContext2D -> m ()
save = error "save only available in ghcjs"

restore :: MonadIO m => CanvasRenderingContext2D -> m ()
restore = error "restore only available in ghcjs"

getTop :: MonadIO m => ClientRect -> m Float
getTop = error "getTop only available in ghcjs"

getLeft :: MonadIO m => ClientRect -> m Float
getLeft = error "getLeft only available in ghcjs"

#endif

