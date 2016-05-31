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
import           Data.Bool
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
import           GHCJS.DOM.EventM         (on, event, stopPropagation, preventDefault)
import qualified GHCJS.DOM.ClientRect     as CR
import           GHCJS.DOM.Element        (getClientTop, getClientLeft)
import           GHCJS.DOM.MouseEvent     (getClientX, getClientY)
import qualified GHCJS.DOM.Types          as T
import           GHCJS.DOM.WheelEvent
import qualified GHCJS.DOM.Element        as E


data Coord = Coord
  { coordX :: Double
  , coordY :: Double
  } deriving (Eq, Show, Ord)

data BoundingBox = BoundingBox
  { bbTopLeft  :: Coord
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
thumbnail (ThumbnailConfig srcImg attrs dZoom dBB) = mdo
  pb <- getPostBuild
  (tnWidget, tn) <- elStopPropagationNS Nothing "div" Wheel $ elAttr' "div" ("class" =: "thumbnail-widget"
                                                                               <> "style" =: "position:relative;") $ mdo

    let thumbPosition = ffor (updated $ siNaturalSize bigPic) $ \(natW, natH) ->
          (0.9 * fI natW :: Double, 0 :: Double)

    zoom  <- foldDyn ($) 1 zooms
    focus <- holdDyn (1 :: Double,1 :: Double) (leftmost [imgLoadPosition, thumbPosUpdates])

    let imgLoadPosition = fmap (\(natW,natH) -> (fI natW / 2, fI natH / 2))
          (tag (current $ siNaturalSize bigPic) (domEvent Load (siEl bigPic)))

    bigPicAttrs <- forDyn sel $ \case
      Nothing -> "class" =: "big-picture"
      Just i  -> "class" =: "big-picture bp-darkened"
              <> "style" =: ""
           -- "filter: blur(2px) brightness(90%); -webkit-filter: blur(2px) brightness(90%);"

    bigPic   <- elAttr "div" ("style" =: "position:absolute;") $
      scaledImage def { sicInitialSource = srcImg
                      , sicAttributes    = bigPicAttrs
                      , sicSetOffset     = uncenteredOffsets bigPic thumbPosUpdates
                      , sicSetScale      = updated zoom
                      }


    selAndSubPics <- foldDyn applySubPicCommand (Just 0, testBoxes) subPicEvents
    sel :: Dynamic t (Maybe Int) <- mapDyn fst selAndSubPics
    subPics :: Dynamic t (Map Int BoundingBox) <- mapDyn snd selAndSubPics

    subPicEvents <- selectMayViewListWithKey sel subPics
      (subPicture srcImg bigPic zoom focus)

    thumbPic :: ScaledImage t <- elAttr "div"
      ("style" =: "position:absolute;opacity:0.5;" <> "class" =: "thumbnail-navigator") $
      scaledImage def { sicInitialSource = srcImg
                      , sicInitialScale  = 0.3
                      }

    let thumbPosUpdates = imageSpaceClick thumbPic

    return $ Thumbnail undefined undefined undefined
  cWheeled <- wrapDomEvent (_el_element tnWidget) (`on` E.wheel) $ do
        ev <- event
        y <- getDeltaY ev
        liftIO $ print y
        preventDefault
        stopPropagation
        return y
  let zooms = fmap (\n z -> z * (1 + n/200)) cWheeled
  return tn

data SubPicCommand = DelSubPic Int
                   | AddSubPic Int BoundingBox
                   | ModifyBox Int BoundingBox
                   | SelBox Int
                   | DeselectBoxes

uncenteredOffsets :: Reflex t => ScaledImage t -> Event t (Double, Double) -> Event t (Double, Double)
uncenteredOffsets bigPic thumbPosUpdates =
  ffor (attach (current $ siNaturalSize bigPic) thumbPosUpdates) $ \((w,h),(x,y)) ->
  (fI w/2 - x, fI h/2 - y)


subPicture :: MonadWidget t m
           => String -- ^ image src
           -> ScaledImage t
           -> Dynamic t Double -- ^ zoom
           -> Dynamic t (Double,Double) -- ^ focus point
           -> Int -- ^ Key
           -> Dynamic t BoundingBox -- ^ Result rect
           -> Dynamic t Bool -- ^ Selected?
           -> m (Event t SubPicCommand)
subPicture srcImg bigPic zoom focus k rect isSel = mdo
  pb <- getPostBuild

  subPicAttrs <- mkSubPicAttrs `mapDyn` isSel
  (e,(img,dels)) <- elDynAttr' "div" subPicAttrs $ do

    img <- scaledImage def { sicInitialSource   = srcImg
                           , sicSetScale        = updated zoom
                           , sicSetOffset       = uncenteredOffsets bigPic $ leftmost [updated focus, tag (current focus) pb]
                           , sicSetBounding     = fmap Just (updated rect)
                           }
    dels <- fmap (DelSubPic k <$) (button "x")
    return (img,dels)

  return $ leftmost [ SelBox k <$ domEvent Click e
                    , dels]

  where mkSubPicAttrs b = "class" =: "sub-picture-top"
                       <> "style" =: ("position:absolute;top:0px;left:0px"
                                      ++ bool unselstyle selstyle b)
          where unselstyle = "border: 1px solid black;"
                selstyle   = "border: 1px solid black; box-shadow: 0px 0px 10px white;"

applySubPicCommand :: (Int, SubPicCommand)
                   -> (Maybe Int, Map Int BoundingBox)
                   -> (Maybe Int, Map Int BoundingBox)
applySubPicCommand (_, DelSubPic k) (_,m) = (Nothing, Map.delete k m)
applySubPicCommand (_, AddSubPic k b) (_,m) = (Just k, Map.insert k b m)
applySubPicCommand (_, ModifyBox k b) (_,m) = (Just k, Map.insert k b m) -- TODO: Ok? insert, not update?
applySubPicCommand (_, SelBox k) (_,m) = (Just k, m)
applySubPicCommand (_, DeselectBoxes) (_,m) = (Nothing,m)

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
  , sicInitialBounding :: Maybe BoundingBox
  , sicSetBounding :: Event t (Maybe BoundingBox)
  }

instance Reflex t => Default (ScaledImageConfig t) where
  def = ScaledImageConfig "" never (constDyn mempty)
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


-- | A widget supporting clipping, zooming, and translation of a source image.
--   Composed of
--     - a parent div fixed to the size of the source image,
--     - a cropping div
--     - the source image
scaledImage :: MonadWidget t m => ScaledImageConfig t -> m (ScaledImage t)
scaledImage (ScaledImageConfig img0 dImg attrs iStyle trans0 dTrans
             scale0 dScale bounding0 dBounding) = mdo
  pb <- getPostBuild

  postImg <- delay 0 (leftmost [pb, () <$ dImg])
  naturalSize :: Dynamic t (Int,Int) <- holdDyn (1 :: Int, 1 :: Int) =<<
    performEvent (ffor (domEvent Load img) $ \() ->
                   (,) <$> (getNaturalWidth htmlImg) <*> (getNaturalHeight htmlImg))

  let htmlImg = castToHTMLImageElement (_el_element img)

  widgetSize <- holdDyn (1,1) =<< performEvent
    (ffor (leftmost [pb, domEvent Load img, resizes]) $ \() -> do
        Just r <- E.getBoundingClientRect (_el_element parentDiv)
        liftM2 (,) (CR.getWidth r) (CR.getHeight r))

  imgSrc     <- holdDyn img0   dImg
  bounding   <- holdDyn bounding0 dBounding
  trans      <- holdDyn trans0 dTrans
  scale      <- holdDyn scale0 dScale

  parentAttrs <- mkParentAttrs `mapDyn` naturalSize

  (resizes,(parentDiv, (img, imgSpace))) <- resizeDetector $
   elDynAttr' "div" parentAttrs $ do

    croppingAttrs  <- mkCroppingAttrs
      `mapDyn` naturalSize `apDyn` bounding `apDyn` scale
      `apDyn`  trans       `apDyn` attrs    `apDyn` iStyle

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
    mkParentAttrs (naturalWid, naturalHei) =
        "class" =: "scaled-image-top"
     <> "style" =: ("position:relative;overflow:hidden;width:" ++ show naturalWid
                    ++ "px;height:" ++ show naturalHei ++ "px;")

    mkCroppingAttrs (naturalWid, naturalHei) bnd scale (offX, offY) attrs extStyle =
     let sizingStyle = case bnd of
           Nothing ->
             let w :: Int = round $ fI naturalWid * scale
                 h :: Int = round $ fI naturalHei * scale
                 x :: Int = round $ offX
                 y :: Int = round $ offY
             in  "width:" ++ show w ++ "px; height: " ++ show h ++ "px; left:" ++ show x ++ "px;top:" ++ show y ++ "px;"
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

         baseStyle = ("position:relative;overflow:hidden;" ++
                      "box-shadow: 10px 10px 10px rgba(0,0,0,0.1);")

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
             in "position:absolute;left:" ++ show x ++ "px;top:" ++ show y ++ "px;"
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

    boundingToCrop (natWid, natHei) (BoundingBox (Coord cX cY) (Coord w h)) =
      Crop { cropLeft   = round (cX - w / 2 :: Double)
           , cropTop    = round (cY - h / 2 :: Double)
           , cropRight  = round (fI natWid - (cX + w / 2 :: Double))
           , cropBottom = round (fI natHei - (cY + h / 2 :: Double))
           }

fI :: (Integral a, RealFrac b) => a -> b
fI = fromIntegral

r2 :: (Real a, Fractional b) => a -> b
r2 = realToFrac

selectMayViewListWithKey :: forall t m k v a. (MonadWidget t m, Ord k)
                         => Dynamic t (Maybe k)
                         -> Dynamic t (Map k v)
                         -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a))
                         -> m (Event t (k,a))
selectMayViewListWithKey sel vals mkChild = do
  let selectionDemux = demux sel
  children <- listWithKey vals $ \k v -> do
    selected <- getDemuxed selectionDemux (Just k)
    selfEvents <- mkChild k v selected
    return $ fmap ((,) k) selfEvents
  fmap switchPromptlyDyn $ mapDyn (leftmost . Map.elems) children

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

testBoxes :: Map Int BoundingBox
testBoxes = Map.fromList
  [ (0, BoundingBox (Coord 100 100) (Coord 200 200))
  , (1, BoundingBox (Coord 300 300) (Coord 200 100))
  ]
