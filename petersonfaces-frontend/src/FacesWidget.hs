{-|
Module: FacesWidget
Description: Specialization of the thumbnail widget for Peterson's face-selection task
Copyright: (c) Greg Hale, 2016
License: BSD3
Maintainer: imalsogreg@gmail.com
Stability: experimental
Portability: GHCJS

-}


{-# language RankNTypes #-}
{-# language DeriveGeneric #-}
{-# language CPP #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language DeriveGeneric #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}

module FacesWidget where

-------------------------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Default
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom
import qualified Data.Map as Mapp
import           Data.Map (Map)
import           GHC.Generics
import           GHCJS.DOM.EventM (on)
#ifdef ghcjs_HOST_OS
import           GHCJS.DOM.Element (getBoundingClientRect)
import           GHCJS.DOM.ClientRect (getTop, getLeft)
#endif
import           GHCJS.DOM.HTMLElement
-- import           GHCJS.DOM.MouseEvent (Mousemove)
-------------------------------------------------------------------------------
import           Canvas2D
import           Thumbnail


data PicUrl = PicUrl T.Text

data FaceLoc = FaceLoc
  { faceCenterX :: Double
  , faceCenterY :: Double
  , faceWidth   :: Double
  , faceHeight  :: Double
  } deriving (Eq, Ord, Show, Generic)


-------------------------------------------------------------------------------
data FacesWidgetConfig t = FacesWidgetConfig
  { facesWidgetConfig_attributes    :: Dynamic t (Map T.Text T.Text)
  , facesWidgetConfig_initialFaces  :: Map Int FaceLoc
  , facesWidgetConfig_setFace       :: Event t (Int, Maybe FaceLoc)
  , facesWidgetConfig_intialPic     :: PicUrl
  , facesWidgetConfig_setPic        :: Event t PicUrl
  , facesWidgetConfig_select        :: Event t Int
  }

data ZoomRect = ZoomRect
  { zrCenter :: (Double, Double)
  , zrWidth  :: Double
  }

data FacesWidget t = FacesWidget
  { facesWidget_faces  :: Dynamic t (Map Int FaceLoc)
  , facesWidget_canvas :: El t
  , facesWidget_selection :: Dynamic t (Maybe (Int, FaceLoc))
  }

instance Reflex t => Default (FacesWidgetConfig t) where
  def = FacesWidgetConfig (constDyn mempty) mempty
                          never (PicUrl "") never never

widgetEventCoords :: MonadWidget t m => El t -> m (Event t (Maybe (Double,Double)))
widgetEventCoords el = do
  let moveFunc (x,y) = do
        Just cr <- getBoundingClientRect (_element_raw el)
        t <- realToFrac <$> (getTop cr  :: IO Float)
        l <- realToFrac <$> (getLeft cr :: IO Float)
        return $ Just (fromIntegral  x - l, fromIntegral y - t)
  performEvent $ fmap (liftIO . moveFunc) (domEvent Mousemove el)

facesWidget :: forall t m.MonadWidget t m => FacesWidgetConfig t -> m (FacesWidget t)
facesWidget (FacesWidgetConfig attrs faces0 dFaces pic0 dPic sel) =

  elClass "div" "faces-widget" $ do
    -- imgAttrs <- holdDyn pic0 dPic >>= mapDyn (\(PicUrl url) -> "src" =: url)
    imgAttrs <- fmap ((\(PicUrl url) -> "src" =: url) <$>) $ holdDyn pic0 dPic
    sourcePic <- fst <$> elDynAttr' "img" imgAttrs (return ())
    sourceCoords <- widgetEventCoords sourcePic

    zoomArea <- canvas $ undefined
    undefined

#ifndef ghcjs_HOST_OS
getBoundingClientRect = undefined
getTop :: MonadIO m => ClientRect -> m Float
getTop = error "getTop only available in ghcjs"

getLeft :: MonadIO m => ClientRect -> m Float
getLeft = error "getLeft only available in ghcjs"
data ClientRect
#endif
