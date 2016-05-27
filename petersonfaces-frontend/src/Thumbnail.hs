{-# language CPP #-}
{-# language RecursiveDo #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}

module Thumbnail where

import           Control.Monad            (liftM2)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Default             (Default, def)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Monoid              ((<>))
import           Reflex.Dom               hiding (restore)
#ifdef ghcjs_HOST_OS
import GHCJS.DOM.HTMLCanvasElement        (getContext, castToHTMLCanvasElement)
import GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D, save, restore, getImageData)
import           GHCJS.DOM.Types          (ImageData, Nullable, nullableToMaybe)
import           GHCJS.Marshal            (fromJSValUnchecked, toJSVal)
import           GHCJS.DOM.Element        (getBoundingClientRect, getClientTop, getClientLeft)
import           GHCJS.DOM.ClientRect     (getTop, getLeft)
#endif
import           GHCJS.DOM.Types          (IsGObject, HTMLCanvasElement, HTMLImageElement)
import           GHCJS.DOM.CanvasRenderingContext2D
import           GHCJS.DOM.HTMLImageElement
import           GHCJS.DOM.EventM         (on, event)
-- import           GHCJS.DOM.BoundingClientRect
import           GHCJS.DOM.Element        (getClientTop, getClientLeft)
import           GHCJS.DOM.MouseEvent     (getClientX, getClientY)
import qualified GHCJS.DOM.Types          as T
import qualified GHCJS.DOM.Element        as E

-- data ZoomPos = ZoomPos
--   { zpZoom    :: Double
--   , zpCenter  :: Coord
--   } deriving (Eq, Show)

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
  , tcZoom        :: Event t BoundingBox
  , tcBoundings   :: Event t (Int, Maybe BoundingBox)
  }

instance Reflex t => Default (ThumbnailConfig t) where
  def = ThumbnailConfig "" (constDyn mempty) never never

data Thumbnail t = Thumbnail
  { tElement   :: El t
  , tBoxes     :: Dynamic t (Map Int BoundingBox)
  , tSelection :: Dynamic t (Maybe (Int, BoundingBox))
  }


-- | A complicated widget used for panning, zooming, and region-selecting within an img
--   Movement is achieved through clicks and mousewheels in a 'picture-in-picture' view
--   in the corner of the widget
thumbnail :: MonadWidget t m => ThumbnailConfig t -> m (Thumbnail t)
thumbnail (ThumbnailConfig srcImg attrs dZoom dBB) = do
  pb <- getPostBuild
  elAttr "div" ("class" =: "thumbnail-widget" <> "style" =: "position:relative;") $ mdo

    let thumbPosition = ffor (updated $ siNaturalSize bigPic) $ \(natW, natH) ->
          (0.9 * fI natW :: Double, 0 :: Double)

    zoom  <- holdDyn (1 :: Double) never
    focus <- holdDyn (1 :: Double,1 :: Double) (leftmost [imgLoadPosition, thumbPosUpdates])

    let imgLoadPosition = fmap (\(natW,natH) -> (fI natW / 2, fI natH / 2))
          (tag (current $ siNaturalSize bigPic) (domEvent Load (siEl bigPic)))

        uncenteredOffsets =
          ffor (attach
                (current $ siNaturalSize bigPic) (leftmost [imgLoadPosition, thumbPosUpdates])) $ \((w,h),(x,y)) ->
            (fI w/2 - x, fI h/2 - y)

    sel <- holdDyn Nothing never

    bigPicAttrs <- forDyn sel $ \case
      Nothing -> "class" =: "big-picture"
      Just i  -> "class" =: "big-picture bp-darkened"

    bigPic   <- elAttr "div" ("style" =: "position:absolute;") $
      scaledImage def { sicInitialSource = srcImg
                      , sicSetOffset     = traceEvent "trace" uncenteredOffsets
                      }

    thumbPic <- elAttr "div" ("style" =: "position:absolute;opacity:0.5;") $
      scaledImage def { sicInitialSource = srcImg
                      , sicInitialScale  = 0.3
                      }



    let thumbPosUpdates = imageSpaceClick thumbPic
    performEvent ((liftIO . putStrLn . ("thumbPosUpdate: " ++ ) . show) <$> thumbPosUpdates )
    performEvent ((liftIO . putStrLn . ("uncenteredPos: " ++ ) . show) <$> uncenteredOffsets )

    return $ Thumbnail undefined undefined undefined

-- | Crop structure contains Ints as a reminder that croppind
--   is always in pixel units of the original image
data Crop = Crop
  { cropLeft   :: Int
  , cropTop    :: Int
  , cropRight  :: Int
  , cropBottom :: Int
  }

instance Default Crop where
  def = Crop 0 0 0 0

data ScaledImageConfig t = ScaledImageConfig
  { sicInitialSource :: String
  , sicSetSource :: Event t String
  , sicAttributes :: Dynamic t (Map String String)
  , sicImgStyle :: Dynamic t String
  , sicInitialOffset :: (Double, Double)
  , sicSetOffset :: Event t (Double, Double)
  , sicInitialScale :: Double
  , sicSetScale :: Event t Double
  , sicInitialCrop :: Crop
  , sicSetCrop :: Event t Crop
  }

instance Reflex t => Default (ScaledImageConfig t) where
  def = ScaledImageConfig "" never (constDyn mempty) (constDyn "") (0,0) never 1 never def never


data ScaledImage t = ScaledImage
  { siImage             :: HTMLImageElement
  , siEl                :: El t
  , siNaturalSize       :: Dynamic t (Int,Int)
  , screenToImageSpace  :: Dynamic t ((Double,Double) -> (Double, Double))
  , imageSpaceClick     :: Event t (Double, Double)
  , imageSpaceMousemove :: Event t (Double, Double)
  , imageSpaceMousedown :: Event t (Double, Double)
  , imageSpaceMouseup   :: Event t (Double, Double)
  }


-- | A widget supporting clipping, zooming, and translation of a source image.
--   Composed of
--     - a parent div fixed to the size of the source image,
--     - a cropping div
--     - the source image
scaledImage :: MonadWidget t m => ScaledImageConfig t -> m (ScaledImage t)
scaledImage (ScaledImageConfig img0 dImg attrs iStyle trans0 dTrans scale0 dScale crop0 dCrop) = mdo
  pb <- getPostBuild

  postImg <- delay 0 (leftmost [pb, () <$ dImg])
  naturalSize :: Dynamic t (Int,Int) <- holdDyn (1 :: Int, 1 :: Int) =<<
    performEvent (ffor (domEvent Load img) $ \() ->
                   (,) <$> (getNaturalWidth htmlImg) <*> (getNaturalHeight htmlImg))

  let htmlImg = castToHTMLImageElement (_el_element img)

  imgSrc <- holdDyn img0   dImg
  crop   <- holdDyn crop0  dCrop
  trans  <- holdDyn trans0 dTrans
  scale  <- holdDyn scale0 dScale

  parentAttrs <- mkParentAttrs `mapDyn` naturalSize

  (parentDiv, (img, imgSpace)) <- elDynAttr' "div" parentAttrs $ do

    croppingAttrs  <- mkCroppingAttrs
      `mapDyn` naturalSize `apDyn` crop `apDyn` scale
      `apDyn` trans        `apDyn` iStyle

    imgAttrs <- mkImgAttrs
      `mapDyn` imgSrc `apDyn` naturalSize `apDyn` scale
      `apDyn`  trans  `apDyn` crop

    (croppingDiv,img) <- elDynAttr' "div" croppingAttrs $
      fst <$> elDynAttr' "img" imgAttrs (return ())

    imgSpace <- mkImgSpace `mapDyn` scale
    return (img, imgSpace)

  clicks <- relativizeEvent (_el_element img) imgSpace E.click
  moves  <- relativizeEvent (_el_element img) imgSpace E.mouseMove
  downs  <- relativizeEvent (_el_element img) imgSpace E.mouseDown
  ups    <- relativizeEvent (_el_element img) imgSpace E.mouseUp

  return $ ScaledImage htmlImg parentDiv naturalSize imgSpace
    clicks moves downs ups

  where
    mkParentAttrs (naturalWid, naturalHei) =
        "class" =: "scaled-image-top"
     <> "style" =: ("position:relative;overflow:hidden;width:" ++ show naturalWid
                    ++ "px;height:" ++ show naturalHei ++ "px;")

    mkCroppingAttrs (naturalWid, naturalHei) cr scale (offX, offY) extStyle =
     let w :: Int = round $ fI (naturalWid - cropLeft cr - cropRight  cr) * scale
         h :: Int = round $ fI (naturalHei - cropTop  cr - cropBottom cr) * scale
         x :: Int = round $ fI (cropLeft cr) * scale + offX
         y :: Int = round $ fI (cropTop  cr) * scale + offY
     in  "style" =: ("width:" ++ show w ++ "px;height:" ++ show h ++ "px;" ++
                     "left:"  ++ show x ++ "px;top:"    ++ show y ++ "px;" ++
                     "position:relative;overflow:hidden;" ++ extStyle ++
                    "box-shadow: 10px 10px 10px rgba(0,0,0,0.1);")

    mkImgAttrs src (naturalWid, naturalHei) scale (offX, offY) cr =
      let w :: Int = round $ fromIntegral naturalWid * scale
          x :: Int = round $ negate $ fI (cropLeft cr) * scale
          y :: Int = round $ negate $ fI (cropTop  cr) * scale
      in  "src"   =: src
       <> "style" =: ("position:absolute;left:" ++ show x ++ "px;top:" ++ show y ++ "px;"
                     ++ "width:" ++ show w ++ "px;")

    mkImgSpace scale = \(x,y) ->
      (x / scale, y / scale)

    relativizeEvent e f eventName = do
      evs <- wrapDomEvent e (`on` eventName) $ do
        ev   <- event
        Just br <- getBoundingClientRect e
        xOff <- (r2 . negate) <$> getLeft br
        yOff <- (r2 . negate) <$> getTop  br
        liftM2 (,) (((+ xOff). fI) <$> getClientX ev) (((+ yOff) . fI) <$> getClientY ev)
      return $ attachWith ($) (current f) evs


fI :: (Integral a, RealFrac b) => a -> b
fI = fromIntegral

r2 :: (Real a, Fractional b) => a -> b
r2 = realToFrac

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

apDyn :: MonadWidget t m => m (Dynamic t (a -> b)) -> Dynamic t a -> m (Dynamic t b)
apDyn mf a = do
  f <- mf
  combineDyn ($) f a
