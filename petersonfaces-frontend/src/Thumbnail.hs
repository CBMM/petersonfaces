{-|
Module: Thumbnail
Description: A picture-in-picture style navigation widget for large images
Copyright: (c) Greg Hale, 2016
License: BSD3
Maintainer: imalsogreg@gmail.com
Stability: experimental
Portability: GHCJS

This module provides a widget for panning and zooming in a large image by
interacting with a smaller 'navigation thumbnail'. For now,
it also allows selecting multiple rectangular regions in the image (this should be factored out somehow)
-}

{-# language CPP #-}
{-# language RecursiveDo #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}

module Thumbnail where

import           Control.Lens
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
import           ScaledImage


data ThumbnailConfig t = ThumbnailConfig
  { tcSourceImage :: String
  , tcAttributes  :: Dynamic t (Map String String)
  , tcZoom        :: Event t BoundingBox
  , tcBoundings   :: Event t (Int, Maybe BoundingBox)
  }

instance Reflex t => Default (ThumbnailConfig t) where
  def = ThumbnailConfig "" (constDyn mempty) never never

data Thumbnail t = Thumbnail
  { tElement          :: El t
  , tBoxes            :: Dynamic t (Map Int BoundingBox)
  , tSelection        :: Dynamic t (Maybe (Int, BoundingBox))
  , tImageNaturalSize :: Dynamic t (Int,Int)
  , tBigPicture       :: ScaledImage t
  }

data Movement = Movement { movementFocus :: (Double,Double)
                         , movementDeltaZoom :: Double
                         }
              deriving (Eq, Show)

data ModelUpdate =
    DelSubPic Int
  | AddSubPic BoundingBox
  | ModifyBox Int BoundingBox
  | SelBox Int
  | DeselectBoxes
  | DoMovement Movement
  | SetGeom (Int,Int)
    deriving (Eq, Show)

data Model t = Model
  { _mFocus   :: (Double,Double)
  , _mZoom    :: Double
  , _mSelect  :: Maybe Int
  , _mSubPics :: Map Int BoundingBox
  , _mGeom    :: (Int,Int)
  } deriving (Eq, Show)

model0 :: Model t
model0 = Model (1,1) 1 Nothing mempty (1,1)

makeLenses ''Model


-- | A complicated widget used for panning, zooming, and region-selecting within an img
--   Movement is achieved through clicks and mousewheels in a 'picture-in-picture' view
--   in the corner of the widget
thumbnail :: forall t m.MonadWidget t m => ThumbnailConfig t -> m (Thumbnail t)
thumbnail (ThumbnailConfig srcImg attrs dZoom dBB) = mdo
  pb <- getPostBuild

  topAttrs <- combineDyn (Map.unionWith (++))
    (constDyn $ "class" =: "thumbnail-widget "
             <> "style" =: "position:relative;")
    attrs

  topResizes <- (fmap . fmap) SetGeom $
    (performEvent (ffor (leftmost [pb, resize, () <$ updated attrs]) $ \() -> do
                      Just r <- getBoundingClientRect $ _el_element tnWidget
                      liftM2 (,) (floor <$> getWidth r) (floor <$> getHeight r)))

  outerScale <- combineDyn (\(natWid,natHei) (wWid,wHei) -> case (wWid,wHei) of
                               (0,0) -> 1
                               _     -> if   wWid == 0
                                        then fI wHei / fI natHei
                                        else fI wWid / fI natWid)
                (tImageNaturalSize tn) topSize

  (resize,(tnWidget, (tn,model))) <- resizeDetectorWithStyle
   "width:100%;height:100%;" $
   elDynAttr' "div" topAttrs $ elStopPropagationNS Nothing "div" Wheel $
   mdo

    display model

    let thumbPosition = ffor (updated $ siNaturalSize bigPic) $ \(natW, natH) ->
          (0.9 * fI natW :: Double, 0 :: Double)

    zoom  <- foldDyn ($) 1 (zooms :: Event t (Double -> Double))
    focus <- holdDyn (1 :: Double,1 :: Double) (leftmost [imgLoadPosition, thumbPosUpdates])

    let imgLoadPosition = fmap (\(natW,natH) -> (fI natW / 2, fI natH / 2))
          (tag (current $ siNaturalSize bigPic) (domEvent Load (siEl bigPic)))

    bigPicAttrs <- forDyn sel $ \case
      Nothing -> "class" =: "big-picture"
      Just i  -> "class" =: "big-picture bp-darkened"
              <> "style" =:
                 ("position:absolute;filter: blur(2px) brightness(90%);"
                  <> " -webkit-filter: blur(2px) brightness(90%);")

    bigPic   <- elDynAttr "div" bigPicAttrs $
      scaledImage def
        { sicInitialSource      = srcImg
        , sicCroppingAttributes = bigPicAttrs
        , sicTopLevelScale      = outerScale
        , sicSetOffset          = uncenteredOffsets bigPic thumbPosUpdates
        , sicSetScale           = updated zoom
        }
    performEvent $ fmap (liftIO . print) (imageSpaceClick bigPic)


    model :: Dynamic t (Model t) <- foldDyn applyModelUpdate model0 (traceEvent "subpic" $ leftmost [fmap snd subPicEvents
                                                                                                    ,AddSubPic testBox <$ pb
                                                                                                    ,topResizes])
    subPicEvents :: Event t (Int, ModelUpdate) <- selectMayViewListWithKey sel subPics
      (subPicture srcImg bigPic zoom focus outerScale)

    thumbScale <- mapDyn (/3) outerScale
    thumbPic :: ScaledImage t <- elAttr "div"
      ("style" =: "position:absolute;opacity:0.5;" <> "class" =: "thumbnail-navigator") $
      scaledImage def { sicInitialSource = srcImg
                      , sicInitialScale  = 1
                      , sicTopLevelScale = thumbScale
                      }

    let thumbPosUpdates = imageSpaceClick thumbPic

    return $ (Thumbnail undefined undefined undefined (siNaturalSize  bigPic) bigPic, model)

  sel :: Dynamic t (Maybe Int) <- mapDyn _mSelect model
  -- subPics :: Dynamic t (Map Int (BoundingBox, Event t Movement)) <- mapDyn _mSubPics selAndSubPics
  subPics :: Dynamic t (Map Int BoundingBox) <- mapDyn _mSubPics model
  topSize :: Dynamic t (Int,Int) <- mapDyn _mGeom model



  cWheeled <- wrapDomEvent (_el_element . siEl . tBigPicture $ tn) (`on` E.wheel) $ do
        ev <- event
        y <- getDeltaY ev
        liftIO $ print y
        preventDefault
        stopPropagation
        pX <- getClientX ev
        pY <- getClientY ev
        -- return (y,(pX,pY))
        return y
  let zooms = fmap (\n z -> z * (1 + n/200)) (traceEvent "wheel" cWheeled)
  return tn


uncenteredOffsets :: Reflex t => ScaledImage t -> Event t (Double, Double) -> Event t (Double, Double)
uncenteredOffsets bigPic thumbPosUpdates =
  ffor (attach (current $ siNaturalSize bigPic) thumbPosUpdates) $ \((w,h),(x,y)) ->
  (fI w/2 - x, fI h/2 - y)



subPicture :: MonadWidget t m
           => String -- ^ image src
           -> ScaledImage t
           -> Dynamic t Double -- ^ zoom
           -> Dynamic t (Double,Double) -- ^ focus point
           -> Dynamic t Double -- ^ Top level (whole-widget) extra scaling
           -> Int -- ^ Key
           -> Dynamic t BoundingBox -- ^ Result rect
           -> Dynamic t Bool -- ^ Selected?
           -> m (Event t ModelUpdate)
subPicture srcImg bigPic zoom focus topScale k rect isSel = mdo
  pb <- getPostBuild

  subPicAttrs <- mkSubPicAttrs `mapDyn` isSel
  (e,(img,dels,dones)) <- elDynAttr' "div" subPicAttrs $ do

    img <- scaledImage def
           { sicInitialSource   = srcImg
           , sicTopLevelScale   = topScale
           , sicSetScale        = updated zoom
           , sicSetOffset       = uncenteredOffsets bigPic $ leftmost [updated focus, tag (current focus) pb]
           , sicSetBounding     = fmap Just . leftmost $ [tag (current rect) pb, updated rect]
           }
    dels  <- fmap (DelSubPic k <$)
           (elAttr "div" ("style" =: "pointer-events:auto;") $ button "x")
    dones <- fmap (DeselectBoxes <$) (elAttr "div" ("style" =: "pointer-events:auto;") $ button "o")
    return (img,dels, dones)

  return $ leftmost [SelBox k <$ gate (not <$> current isSel)
                                      (domEvent Click (siEl img))
                    , traceEvent "Del" dels
                    , traceEvent "DoneClick" dones
                    ]

  where mkSubPicAttrs b = "class" =: "sub-picture-top"
                       <> "style" =: ("pointer-events:none;position:absolute;top:0px;left:0px;"
                                      ++ bool unselstyle selstyle b)
          where unselstyle = "border: 1px solid black;"
                selstyle   = "border: 1px solid black; box-shadow: 0px 0px 10px white;"


-------------------------------------------------------------------------------
applyModelUpdate :: ModelUpdate -> Model t -> Model t
applyModelUpdate (DelSubPic k  ) m = m & over mSubPics (Map.delete k)
                                         & set  mSelect  Nothing
applyModelUpdate (AddSubPic b  ) m =
  let k = maybe 0 (succ . fst . fst) (Map.maxViewWithKey $ _mSubPics m)
  in m & over mSubPics (Map.insert k b) -- (Just k, Map.insert k b m)
       & set  mSelect (Just k)
applyModelUpdate (ModifyBox k b) m = undefined -- (Just k, Map.insert k b m) -- TODO: Ok? insert, not update?
applyModelUpdate (SelBox k     ) m = m & set mSelect (Just k)
applyModelUpdate (DeselectBoxes) m = m & set mSelect Nothing
applyModelUpdate (DoMovement mv) m = undefined
applyModelUpdate (SetGeom g)     m = m & set mGeom g



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

-- data CanvasRenderingContext2D
-- data ImageData
-- data ClientRect = ClientRect
--   deriving Show

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

-- getBoundingClientRect :: MonadIO m => a -> m (Maybe ClientRect)
-- getBoundingClientRect = error "getBoundingClientRect only available in ghcjs"

getTop :: MonadIO m => ClientRect -> m Float
getTop = error "getTop only available in ghcjs"

getLeft :: MonadIO m => ClientRect -> m Float
getLeft = error "getLeft only available in ghcjs"

#endif

data ZoomInfo = ZoomInfo
  { zoomFocus :: (Double, Double)
  , zoomZoom  :: Double
  } deriving (Eq, Show)

testBox :: BoundingBox
testBox = BoundingBox (Coord 100 100) (Coord 200 200)

-- testBoxes :: Reflex t => Map Int (BoundingBox, Event t ZoomInfo)
-- testBoxes = Map.fromList
--   [ (0, (BoundingBox (Coord 100 100) (Coord 200 200), never))
--   ]
