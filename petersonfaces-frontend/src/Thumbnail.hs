{-# language CPP #-}
{-# language RecursiveDo #-}
{-# language KindSignatures #-}
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
import           GHCJS.DOM.Element        (getClientTop, getClientLeft)

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


-- | A complicated widget used for panning, zooming, and region-selecting within an img
--   Movement is achieved through clicks and mousewheels in a 'picture-in-picture' view
--   in the corner of the widget
thumbnail :: MonadWidget t m => ThumbnailConfig t -> m (Thumbnail t)
thumbnail (ThumbnailConfig srcImg attrs dZoom dBB) =
  divClass "thumbnail-widget" $ do
    widgetAttrs <- return attrs -- Add default attributes?
    elDynAttr "div" widgetAttrs $ do
      undefined

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
  , sicInitialOffset :: (Double, Double)
  , sicSetOffset :: Event t (Double, Double)
  , sicInitialScale :: Double
  , sicSetScale :: Event t Double
  , sicInitialCrop :: Crop
  , sicSetCrop :: Event t Crop
  }

instance Reflex t => Default (ScaledImageConfig t) where
  def = ScaledImageConfig "" never (constDyn mempty) (0,0) never 1 never def never


data ScaledImage t = ScaledImage
  { siImage         :: HTMLImageElement
  , siEl            :: El t
  , siNaturalCoords :: Dynamic t ((Int,Int) -> (Double,Double)) -- TODO Don't like this
  }


-- | A widget supporting clipping, zooming, and translation of a source image.
--   Composed of
--     - a parent div fixed to the size of the source image,
--     - an intermiate div used as a clipping box
--     - the source image
scaledImage :: MonadWidget t m => ScaledImageConfig t -> m (ScaledImage t)
scaledImage (ScaledImageConfig img0 dImg attrs trans0 dTrans scale0 dScale crop0 dCrop) = mdo
  pb <- getPostBuild

  postImg <- delay 0 (leftmost [pb, () <$ dImg])
  naturalSize :: Dynamic t (Int,Int) <- holdDyn (1 :: Int, 1 :: Int) =<<
    performEvent (ffor (domEvent Load img) $ \() ->
                   (,) <$> (getNaturalWidth htmlImg) <*> (getNaturalHeight htmlImg))

  display naturalSize

  let htmlImg = castToHTMLImageElement (_el_element img)

  imgSrc <- holdDyn img0   dImg
  crop   <- holdDyn crop0  dCrop
  trans  <- holdDyn trans0 dTrans
  scale  <- holdDyn scale0 dScale

  clippingDivAttrs <- mkClippingDivAttrs

  parentAttrs <- mkParentAttrs `mapDyn` naturalSize `apDyn` boxSize

  (resized, (parentDiv, (_,img))) <- resizeDetector $ elDynAttr' "div" parentAttrs $ do

    (clipDiv, img) <- elDynAttr' "div" clippingDivAttrs $ do

      imgAttrs <- mkImgAttrs `mapDyn` imgSrc `apDyn` naturalSize `apDyn` scale `apDyn` trans
      img <- fst <$> elDynAttr' "img" imgAttrs (return ())
      return img

    return (clipDiv, img)

  boxSize <- return naturalSize -- TODO totally wrong

  return $ ScaledImage htmlImg parentDiv (constDyn (const (1,1)))

  where
    mkParentAttrs (naturalWid, naturalHei) boxSize =
        "class" =: "scaled-image-top"
     <> "style" =: ("overflow:hidden;width:" ++ show naturalWid ++ "px;height:" ++ show naturalHei ++ "px;")
    mkClippingDivAttrs  = return $ constDyn mempty
    mkImgAttrs src (naturalWid, naturalHei) scale (offX, offY) =
      let w :: Int = floor $ fromIntegral naturalWid * scale
          x :: Int = floor offX
          y :: Int = floor offY
      in  "src"   =: src
       <> "style" =: ("width:" ++ show w ++ "px;position:absolute;top:" ++ show y ++ "px;left:" ++  show x ++ "px;")

  -- imgAttrs <- combineDyn (\s a -> a <> "class" =: "scaled-image" <> "src" =: s) srcImg attrs
  -- (resized, imgEl) <- resizeDetector $ fst <$> elDynAttr' "img" imgAttrs (return ())
  -- let img = castToHTMLImageElement (_el_element imgEl)
  --     imgEvents = leftmost [pb, resized]
  --     fI = fromIntegral :: Int -> Double
  -- imgInfo <- performEvent $ ffor imgEvents $ \() -> liftIO $ do
  --   widNat <- getNaturalWidth img
  --   heiNat <- getNaturalHeight img
  --   wid    <- getWidth img
  --   hei    <- getHeight img
  --   return ((widNat,heiNat),(wid,hei))
  -- dynText =<< holdDyn "" (fmap show imgInfo)
  -- mouseCoordsFn <- holdDyn (\(x,y) -> (fI x, fI y)) $ ffor imgInfo $
  --   \((widNat,heiNat),(wid,hei)) (mX,mY) ->
  --   let x' = fI mX * fI widNat / fI wid
  --       y' = fI mY * fI heiNat / fI hei
  --   in  (x',y')
  -- return $ ScaledImage img imgEl mouseCoordsFn


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
