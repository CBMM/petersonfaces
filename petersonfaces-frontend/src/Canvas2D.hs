{-|
Module: Canvas2D
Description: reflex-dom interface to an html5 canvas
Copyright: (c) Greg Hale, 2016
License: BSD3
Maintainer: imalsogreg@gmail.com
Stability: experimental
Portability: GHCJS

This module exposes an HTML5 canvas through a reflex-dom interface.
Note: It's completely untested and unused.

-}

{-# language CPP #-}
{-# language RecursiveDo #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

module Canvas2D where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Reflex.Dom hiding (restore)
#ifdef ghcjs_HOST_OS
import GHCJS.DOM.HTMLCanvasElement (getContext, castToHTMLCanvasElement)
import GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D, save, restore, getImageData)
import GHCJS.DOM.Types (ImageData, Nullable, nullableToMaybe)
import GHCJS.Marshal (fromJSValUnchecked, toJSVal)
#endif
import GHCJS.DOM.Types (IsGObject, HTMLCanvasElement)
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.ImageData


data CanvasConfig t = CanvasConfig
  { canvasConfig_attributes         :: Dynamic t (Map String String)
  , canvasConfig_localContextAction :: Event t (CanvasRenderingContext2D -> IO ())
  , canvasConfig_initialContext     :: CanvasRenderingContext2D
  , canvasConfig_modifyContext      :: Event t (CanvasRenderingContext2D -> IO ())
  }

data Canvas t m = Canvas
  { canvas_el           :: El t
  , canvas_context      :: Dynamic t CanvasRenderingContext2D
  , canvas_getImageData :: Event t (Float, Float, Float, Float) -> m (Event t (Maybe ImageData))
  }


canvas :: forall t m.MonadWidget t m => CanvasConfig t -> m (Canvas t m)
canvas (CanvasConfig attrs localCtx ctx0 touchCtx) = do

  (canvasEl, children) <- elDynAttr' "canvas" attrs (return ())
  let canv = castToHTMLCanvasElement $ _el_element canvasEl

  rec ctx <- holdDyn ctx0 touchedCtx

      touchedCtx <- performEvent $ ffor (attach (current ctx) touchCtx) $ \(c,mkCtx) -> liftIO $ do
        cOld <- getContext canv "2d"
        mkCtx =<< fromJSValUnchecked cOld :: IO ()
        fromJSValUnchecked =<< getContext canv "2d"

  performEvent $ ffor (attach (current ctx) localCtx) $ \(c,act) -> liftIO $ do
    save c
    getContext canv "2d" >>= \jv -> fromJSValUnchecked jv >>= act
    restore c

  let getCtx triggers = performEvent $ ffor (attach (current ctx) triggers) $ \(c, (x,y,w,h)) -> liftIO $ do
        getImageData c x y w h
        -- return $ nullableToMaybe i

  return $ Canvas canvasEl ctx getCtx


#ifndef ghcjs_HOST_OS
fromJSValUnchecked = error ""
toJSVal = error ""

data CanvasRenderingContext2D
data ImageData

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

#endif
