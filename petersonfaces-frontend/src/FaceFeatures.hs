{-|
Module: FaceFeatures
Description: A widget for labeling features on a face image
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
{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}


module FaceFeatures where

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
import Common

data FaceAttributes t = FaceAttributes {
    faBounding :: BoundingBox
  , faFeatures :: Dynamic t (Map.Map Int FaceFeature)
  }

type FaceFeature = (String, (Double,Double))
type FaceUpdate = Map Int (Maybe FaceFeature)

faceWidget :: forall t m.MonadWidget t m => BoundingBox -> ScaledImage t -> m (FaceAttributes t)
faceWidget bb@(BoundingBox (Coord bX0 bY0) (Coord bX1 bY1)) img = mdo
  pb <- getPostBuild
  -- rect <- constDyn bb
  let (wid,hei) = (bX1 - bX0, bY1 - bY0)
      -- relativeFeatures f =
      --   Map.fromList [(0,("Eye",   f (bX0 + wid/2 - 50, bY0 + hei/2 - 20)))
      --                ,(1,("Eye",   f (bX0 + wid/2 + 50,  bY0 + hei/2 - 20)))
      --                ,(2,("Nose",  f (bX0 + wid/2, bY0 + hei/2)))
      --                ,(3,("Mouth", f (bX0 + wid/2, bY0 + hei/2 + 50)))
      --                ]

      features0 =
        Map.fromList [(0,("Eye",   (bX0 + wid/2 - 50, bY0 + hei/2 - 20)))
                     ,(1,("Eye",   (bX0 + wid/2 + 50,  bY0 + hei/2 - 20)))
                     ,(2,("Nose",  (bX0 + wid/2, bY0 + hei/2)))
                     ,(3,("Mouth", (bX0 + wid/2, bY0 + hei/2 + 50)))
                     ]
      -- defFeatures :: Event t FaceUpdate = ffor (tag (current $ imageToScreenSpace img) pb) $ \f ->
      --   fmap Just (relativeFeatures f)


  (facePartEvents, selection) <- listSelfDeleting features0 never never (faceFeatureWidget bb img)
  -- faceParts :: Dynamic t (Map Int FaceFeature) <- fmap (traceDyn "FACEPARTS") $ joinDynThroughMap <$> mapDyn (fmap (\(a,_,_,_) -> a)) facePartEvents

  -- faceDeletions <- fmap ((fmap . fmap) (const Nothing) . switchPromptlyDyn) $
  --   mapDyn (mergeMap . fmap snd) facePartEvents
  return $ FaceAttributes bb undefined -- faceParts

data MouseInfo t = MouseInfo {
    mouseDowns :: Event t (Int,Int)
  , mouseMoves :: Event t (Int,Int)
  , mouseUps   :: Event t ()
  }

faceFeatureWidget :: forall t m. MonadWidget t m
                  => BoundingBox
                  -> ScaledImage t
                  -> Int
                  -> FaceFeature
                  -> Event t FaceFeature
                  -> m ((Dynamic t FaceFeature, MouseInfo t) , Event t DeselectMe, Event t DeleteMe, Event t SelectMe)
faceFeatureWidget bb img k f0 dF = mdo
  pb <- getPostBuild
  -- ff :: Dynamic t FaceFeature <- fmap (traceDyn "TEST") $ holdDyn f0 $ leftmost [ dF, fUpdates ]
  ff :: Dynamic t FaceFeature <- holdDyn f0 dF -- $ fUpdates -- leftmost [ traceEvent "dF" dF, fUpdates ]

  let (divWid,divHei) :: (Double,Double) = (20,20)
  featureAttrs <- forDyn ff $ \(nm,(x,y)) ->
    let (wX,wY) = (x,y)
        xOff = "left: " <> T.pack (show (wX - divWid/2)) <> "px;"
        yOff = "top: " <> T.pack (show (wY - divHei/2)) <> "px;"
        wid  = "width: " <> T.pack (show divWid) <> "px;"
        hei  = "height: " <> T.pack (show divHei) <> "px;"

        styl = "position: absolute; border: 1px solid rgba(0,0,0,0.5); background-color: hsla(0,100%,100%,0.25); border-radius:200px;"
    in  "style" =: T.unpack ( xOff <> yOff <> wid <> hei <> styl) <> "class" =: "face-feature"


  -- The top-level div for the feature label
  (ffDiv, dels) <- elDynAttr' "div" featureAttrs $
   elAttr "div" ("class" =: "face-feature-container" <> "style" =: "position:absolute;") $ do
    elAttr "div" ("class" =: "face-feature-dot"
                 <> "style" =: ("position:absolute; top: " <>
                                show (divHei/2 - 2) <> "px; left: " <>
                                show (divWid/2 - 2) <> "px; width:4px; height:4px;" <>
                                "background-color:black;border-radius:5px;")) (return ())
    closes <- elAttr "div" ("class" =: "face-feature-info" <> "style" =: "position: absolute; left: 20px;top:-20px;") $
      elAttr "div" ("class" =: "face-feature-delete" <> "style" =:
                    "position:absolute; background-color:rgba(255,255,255,0.5); border-radius:2px; padding: 1px; ") $ do
        text (fst f0)
        fmap (domEvent Click . fst) $
          elAttr' "span" ("class" =: "button" <> "style" =:
                          ("color:hsla(3,50%,50%,1); text-shadow:1px 1px rgba(0,0,0,0.5); "<>
                           "border-left: 1px solid gray; margin-left: 3px;")) $ text "✗"
    return closes

  let moves  = domEvent Mousemove ffDiv
      sels   = domEvent Mousedown ffDiv
      desels = domEvent Mouseup   ffDiv
      mouseInfo = MouseInfo sels moves (() <$ desels)

  return ((ff, mouseInfo), DeselectMe <$ desels, DeleteMe <$ dels, SelectMe <$ sels) -- TODO: Catch delete events



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
