{-# language CPP #-}
{-# language RecursiveDo #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

module Thumbnail where

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
import GHCJS.DOM.Element (getBoundingClientRect, getClientTop, getClientLeft)
import           GHCJS.DOM.ClientRect (getTop, getLeft)
#endif
import GHCJS.DOM.Types (IsGObject, HTMLCanvasElement, HTMLImageElement)
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.HTMLImageElement
import GHCJS.DOM.Element (getClientTop, getClientLeft)
-- import GHCJS.DOM.ImageData

data ZoomPos = ZoomPos
  { zpZoom    :: Double
  , zpCenter  :: Coord
  } deriving (Eq, Show)

data Coord = Coord
  { coordX :: Double
  , coordY :: Double
  } deriving (Eq, Show, Ord)

data BoundingBox = BoundingBox
  { bbTopLeft :: Coord
  , bbBotRight :: Coord
  } deriving (Eq, Show, Ord)

data ThumbnailConfig t = ThumbnailConfig
  { tcSourceImage :: String
  , tcAttributes  :: Dynamic t (Map String String)
  , tcZoom        :: Event t ZoomPos
  , tcBoundings   :: Event t (Int, Maybe BoundingBox)
  }

data Thumbnail t = Thumbnail
  { tElement :: El t
  , tBoxes   :: Dynamic t (Map Int BoundingBox)
  }


thumbnail :: MonadWidget t m => ThumbnailConfig t -> m (Thumbnail t)
thumbnail (ThumbnailConfig srcImg attrs dZoom dBB) =
  divClass "thumbnail-widget" $ do
    widgetAttrs <- return attrs -- Add default attributes?
    elDynAttr "div" widgetAttrs $ do
      undefined

data ScaledImageConfig t = ScaledImageConfig
  { sicSource :: Dynamic t String
  , sicAttributes :: Dynamic t (Map String String)
  }

data ScaledImage t = ScaledImage
  { siImage         :: HTMLImageElement
  , siEl            :: El t
  , siNaturalCoords :: Dynamic t ((Int,Int) -> (Double,Double))
  }


scaledImage :: MonadWidget t m => ScaledImageConfig t -> m (ScaledImage t)
scaledImage (ScaledImageConfig srcImg attrs) = do
  pb <- getPostBuild
  imgAttrs <- combineDyn (\s a -> a <> "class" =: "scaled-image" <> "src" =: s) srcImg attrs
  (resized, imgEl) <- resizeDetector $ fst <$> elDynAttr' "img" imgAttrs (return ())
  let img = castToHTMLImageElement (_el_element imgEl)
      imgEvents = leftmost [pb, resized]
      fI = fromIntegral :: Int -> Double
  imgInfo <- performEvent $ ffor imgEvents $ \() -> liftIO $ do
    -- Just rect   <- getBoundingClientRect (_el_element imgEl)
    -- x      <- floor <$> getLeft rect
    -- y      <- floor <$> getTop rect
    widNat <- getNaturalWidth img
    heiNat <- getNaturalHeight img
    wid    <- getWidth img
    hei    <- getHeight img
    return ((widNat,heiNat),(wid,hei))
    -- return ((x,y), (widNat,heiNat),(wid,hei))
  dynText =<< holdDyn "" (fmap show imgInfo)
  mouseCoordsFn <- holdDyn (\(x,y) -> (fI x, fI y)) $ ffor imgInfo $
    \((widNat,heiNat),(wid,hei)) (mX,mY) ->
    let x' = fI mX * fI widNat / fI wid
        y' = fI mY * fI heiNat / fI hei
    in  (x',y')
  return $ ScaledImage img imgEl mouseCoordsFn


#ifndef ghcjs_HOST_OS
fromJSValUnchecked = error ""
toJSVal = error ""

data CanvasRenderingContext2D
data ImageData
data ClientRect = ClientRect
  deriving Show

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

getBoundingClientRect :: MonadIO m => a -> m (Maybe ClientRect)
getBoundingClientRect = error "getBoundingClientRect only available in ghcjs"

getTop :: MonadIO m => ClientRect -> m Float
getTop = error "getTop only available in ghcjs"

getLeft :: MonadIO m => ClientRect -> m Float
getLeft = error "getLeft only available in ghcjs"

#endif
