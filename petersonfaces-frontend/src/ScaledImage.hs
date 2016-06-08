{-# language CPP #-}
{-# language RecursiveDo #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}

module ScaledImage (

  ScaledImageConfig(..),
  ScaledImage(..),
  scaledImage,
  BoundingBox (..),
  Coord(..),
  apDyn,
  fI,
  r2,
  CanvasRenderingContext2D(..),
  ImageData(..),
  ClientRect(..),
  getWidth,
  getHeight,
  getBoundingClientRect
)where

import           Control.Monad            (liftM2)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Bool
import           Data.Default             (Default, def)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Monoid              ((<>))
import           Reflex.Dom               hiding (restore)
#ifdef ghcjs_HOST_OS
import GHCJS.DOM.HTMLCanvasElement        (getContext, castToHTMLCanvasElement)
import GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D, save, restore, getImageData)
import           GHCJS.DOM.Types          (ImageData, ClientRect, Nullable, nullableToMaybe)
import           GHCJS.Marshal            (fromJSValUnchecked, toJSVal)
import           GHCJS.DOM.Element        (getBoundingClientRect, getClientTop, getClientLeft)
import           GHCJS.DOM.ClientRect     (getTop, getLeft, getWidth, getHeight)
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


data Coord = Coord
  { coordX :: Double
  , coordY :: Double
  } deriving (Eq, Show, Ord)

data BoundingBox = BoundingBox
  { bbTopLeft  :: Coord
  , bbBotRight :: Coord
  } deriving (Eq, Show, Ord)


data ScaledImageConfig t = ScaledImageConfig
  { sicInitialSource :: String
  , sicSetSource :: Event t String
  , sicTopLevelScale :: Dynamic t Double
  , sicTopLevelAttributes :: Dynamic t (Map String String)
  , sicCroppingAttributes :: Dynamic t (Map String String)
  , sicImgStyle :: Dynamic t String
  , sicInitialOffset :: (Double, Double)
  , sicSetOffset :: Event t (Double, Double)
  , sicInitialScale :: Double
  , sicSetScale :: Event t Double
  , sicInitialBounding :: Maybe BoundingBox
  , sicSetBounding :: Event t (Maybe BoundingBox)
  }

instance Reflex t => Default (ScaledImageConfig t) where
  def = ScaledImageConfig "" never (constDyn 1) (constDyn mempty) (constDyn mempty)
    (constDyn "") (0,0) never 1 never def never


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

-- TODO: This kind of pattern seems to come up a lot. I began trying to generalize it here,
--       but want to make sure it works like this at all the potential use sites before finishing
--       and plugging it in
-- sizedWidget :: MonadWidget t m
--             => (Dynamic t (Map String String) -> m a)
--             -> Event t (Double, Double)
--             -> m (Dynamic t (Double, Double), a)
-- sizedWidget ma setSize = do
--   (resizeEvents, a) <- ma =<< (forDyn 

--   size <- holdDyn (1,1) =<< performEvent (ffor (leftmost [pb, fmap Just setSize, ])))
--   holdDyn (1,1) =<< (

-- | A widget supporting clipping, zooming, and translation of a source image.
--   Composed of
--     - a parent div fixed to the size of the source image,
--     - a cropping div
--     - the source image
scaledImage :: MonadWidget t m => ScaledImageConfig t -> m (ScaledImage t)
scaledImage (ScaledImageConfig img0 dImg topScale topAttrs cropAttrs iStyle trans0 dTrans
             scale0 dScale bounding0 dBounding) = mdo
  pb <- getPostBuild

  postImg <- delay 0 (leftmost [pb, () <$ dImg])
  naturalSize :: Dynamic t (Int,Int) <- holdDyn (1 :: Int, 1 :: Int) =<<
    performEvent (ffor (domEvent Load img) $ \() ->
                   (,) <$> (ImageElement.getNaturalWidth htmlImg)
                       <*> (ImageElement.getNaturalHeight htmlImg))

  let htmlImg = ImageElement.castToHTMLImageElement (_el_element img)

  -- widgetSize <- holdDyn (1 :: Double, 1 :: Double) =<< performEvent
  --   (ffor (leftmost [pb, domEvent Load img, resizes]) $ \() -> do
  --       Just r <- getBoundingClientRect (_el_element parentDiv)
  --       liftM2 (,) (r2 <$> getWidth r) (r2 <$> getHeight r))
  -- outerScale <- combineDyn (\(natWid, natHei) (wWid, wHei) ->
  --                            min (fI natWid / wWid) (fI natHei / wHei))
  --   naturalSize widgetSize

  imgSrc     <- holdDyn img0 dImg
  bounding   <- holdDyn bounding0 dBounding
  trans      <- holdDyn trans0 dTrans
  innerScale <- holdDyn scale0 dScale
  scale      <- combineDyn (*) innerScale topScale

  parentAttrs <- mkTopLevelAttrs `mapDyn` naturalSize `apDyn` topAttrs `apDyn` topScale

  (resizes,(parentDiv, (img, imgSpace))) <- resizeDetector $
   elDynAttr' "div" parentAttrs $ do

    croppingAttrs  <- mkCroppingAttrs
      `mapDyn` naturalSize `apDyn` bounding  `apDyn` scale
      `apDyn`  trans       `apDyn` cropAttrs `apDyn` iStyle

    imgAttrs <- mkImgAttrs
      `mapDyn` imgSrc `apDyn` naturalSize `apDyn` scale
      `apDyn`  trans  `apDyn` bounding

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
    mkTopLevelAttrs (naturalWid, naturalHei) topAttrs topScale =
      let defAttrs =
               "class" =: "scaled-image-top"
            <> "style" =: ("pointer-events:none;position:relative;overflow:hidden;width:" ++ show (fI naturalWid * topScale)
                           ++ "px;height:" ++ show (fI naturalHei * topScale) ++ "px;")
      in Map.unionWith (++) defAttrs topAttrs

    mkCroppingAttrs (natWid, natHei) bnd scale (offX, offY) attrs extStyle =
     let sizingStyle = case bnd of
           Nothing ->
             let w :: Int = round $ fI natWid * scale
                 h :: Int = round $ fI natHei * scale
                 x :: Int = round $ offX
                 y :: Int = round $ offY
             in  "width:" ++ show w ++ "px; height: " ++ show h ++
                 "px; left:" ++ show x ++ "px;top:" ++ show y ++ "px;"
           Just (BoundingBox (Coord cX cY) (Coord bW bH)) ->
             let w :: Int = round $ bW * scale
                      -- round $ fI (naturalWid - cropLeft cr - cropRight  cr) * scale
                 h :: Int = round $ bH * scale
                      -- round $ fI (naturalHei - cropTop  cr - cropBottom cr) * scale
                 x :: Int = round $ (bW / 2) * scale + offX
                      -- round $ fI (cropLeft cr) * scale + offX
                 y :: Int = round $ (bH / 2) * scale + offY
                      -- round $ fI (cropTop  cr) * scale + offY
             in ("width:" ++ show w ++ "px;height:" ++ show h ++ "px;" ++
                      "left:"  ++ show x ++ "px;top:"    ++ show y ++ "px;")

         baseStyle = "pointer-events:auto;position:relative;overflow:hidden;"

         style = case Map.lookup "style" attrs of
           Nothing -> baseStyle ++ sizingStyle ++ extStyle
           Just s  -> baseStyle ++ sizingStyle ++ s ++ extStyle
     in Map.insert "style" style ("class" =: "cropping-div" <> attrs)

    mkImgAttrs src (naturalWid, naturalHei) scale (offX, offY) bb  =
     let posPart = case bb of
           Nothing ->
             let w :: Int = round $ fI naturalWid * scale
                 h :: Int = round $ fI naturalHei * scale
             in "width:" ++ show w ++ "px; height: " ++ show h ++ "px; position:absolute; left: 0px; top 0px;"
           Just (BoundingBox (Coord cX cY) (Coord bW bH)) ->
             let w :: Int = round $ fromIntegral naturalWid * scale
                 x :: Int = round $ negate (bW / 2) * scale
                      -- round $ negate $ fI (cropLeft cr) * scale
                 y :: Int = round $ negate (bH / 2) * scale
                      -- round $ negate $ fI (cropTop  cr) * scale
             in "pointer-events:auto;position:absolute;left:" ++ show x ++ "px;top:" ++ show y ++ "px;"
                ++ "width:" ++ show w ++ "px;"
      in   "src"   =: src
        <> "style" =: posPart

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

getWidth :: MonadIO m => ClientRect -> m Float
getWidth = error "getWidth only available in ghcjs"

getHeight :: MonadIO m => ClientRect -> m Float
getHeight = error "getHeight only available in ghcjs"

#endif

apDyn :: MonadWidget t m => m (Dynamic t (a -> b)) -> Dynamic t a -> m (Dynamic t b)
apDyn mf a = do
  f <- mf
  combineDyn ($) f a
