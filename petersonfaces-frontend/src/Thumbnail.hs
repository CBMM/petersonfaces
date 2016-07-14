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

import           Control.Arrow
import           Control.Lens
import           Control.Monad            (liftM2)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Bool
import           Data.Default             (Default, def)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Monoid              ((<>))
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


data ModelUpdate =
    DelSubPic Int
  | AddSubPic BoundingBox
  | ModifyBox Int BoundingBox
  | SelBox Int
  | DeselectBoxes
  | SetZoom Double
  | SetFocus (Double, Double)
  | ZoomAbout Double (Double, Double)
  | SetGeom (Int,Int)
  | SetNatSize (Int,Int)
    deriving (Eq, Show)

data Model = Model
  { _mFocus   :: (Double,Double)
  , _mZoom    :: Double
  , _mSelect  :: Maybe Int
  , _mSubPics :: Map Int BoundingBox
  , _mGeom    :: (Int,Int)
  , _mNatSize :: (Int,Int)
  } deriving (Eq, Show)

model0 :: Model
model0 = Model (0,0) 1 Nothing mempty (0,0) (0,0)

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

  firstLoad <- headE $ domEvent Load $ siImgEl $ tBigPicture tn

  natSizes <- mapDyn SetNatSize $ nubDyn (tImageNaturalSize tn)

  topResizes <- (fmap . fmap) SetGeom $
    (performEvent (ffor (leftmost [pb, () <$ updated attrs]) $ \() -> do
                      Just r <- getBoundingClientRect $ _el_element tnWidget
                      liftM2 (,) (floor <$> getWidth r) (floor <$> getHeight r)))


  outerScale <- combineDyn (\(natWid,natHei) (wWid,wHei) -> case (wWid,wHei) of
                               (0,0) -> 1
                               _     -> if   wWid == 0
                                        then fI wHei / fI natHei
                                        else fI wWid / fI natWid)
                (tImageNaturalSize tn) topSize

  display model
  (resize,(tnWidget, (tn,model))) <- resizeDetectorWithStyle
   "width:100%;height:100%;" $
   elDynAttr' "div" topAttrs $ elStopPropagationNS Nothing "div" Wheel $
   mdo


    let thumbPosition = ffor (updated $ siNaturalSize bigPic) $ \(natW, natH) ->
          (0.9 * fI natW :: Double, 0 :: Double)

    zoom  <- mapDyn _mZoom  model
    focus <- mapDyn _mFocus model
    zoomPos <- forDyn model $ \m -> (_mZoom m, _mFocus m) -- TODO cleanup

    -- let imgLoadPosition = fmap (\(natW,natH) -> (fI natW / 2, fI natH / 2))
    --       (tag (current $ siNaturalSize bigPic) (domEvent Load (siEl bigPic)))

    bigPicAttrs <- forDyn sel $ \case
      Nothing -> "class" =: "big-picture" <> "style" =: "position:absolute"
      Just i  -> "class" =: "big-picture bp-darkened"
              <> "style" =:
                 ("position:absolute;filter: blur(2px) brightness(90%);"
                  <> " -webkit-filter: blur(2px) brightness(90%); opacity:0.5;")

    bigPic   <- elDynAttr "div" bigPicAttrs $
      scaledImage def
        { sicInitialSource      = srcImg
        , sicCroppingAttributes = bigPicAttrs
        , sicTopLevelScale      = outerScale
        , sicSetOffset          = uncenteredOffsets bigPic thumbPosUpdates
        , sicSetScale           = updated zoom
        }
    performEvent $ fmap (liftIO . print) (imageSpaceClick bigPic)
    performEvent $ ffor (attach (current model) $ imageSpaceClick bigPic) $ \(m, (x,y)) ->
      liftIO (putStrLn $ "widgetspace: " ++ show (imageSpaceToWidgetSpace m (x,y)))

    let okToAddSelection = fmap (== Nothing) (current sel)
        newSelectionPos (z,(wid,hei)) (x,y) =
            let (boxWid,boxHei) = (fI wid / z / 4, fI hei / z / 4)
                topLeft  = Coord (max 0 $ x - boxWid/2)            (max 0 $ y - boxHei/2)
                botRight = Coord (min (fI wid - 1) $ x + boxWid/2) (min (fI hei - 1) $ y + boxHei/2)
            in  AddSubPic (BoundingBox topLeft botRight)
        addSel = gate okToAddSelection $
                 attachWith newSelectionPos
                            ((,) <$> current zoom <*> current (siNaturalSize bigPic))
                            (imageSpaceDblClick bigPic)

    pb' <- delay 0.5 pb

    model :: Dynamic t Model <- foldDyn applyModelUpdate model0
        (traceEvent "subpic" $ leftmost [fmap SetFocus thumbPosUpdates
                                        ,updated natSizes
                                        ,fmap snd subPicEvents
                                        ,AddSubPic testBox <$ pb'
                                        ,topResizes
                                        ,addSel
                                        -- ,fmap (SetZoom . fst) zooms -- TODO replace this with ZoomAbout
                                        ,fmap (uncurry ZoomAbout) zooms
                                        -- ,fmap imgLoadPosition
                                        ])

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
        zooms           = fmap (first (/ 200)) $ imageSpaceWheel bigPic

    return $ (Thumbnail undefined undefined undefined (siNaturalSize  bigPic) bigPic, model)

  sel :: Dynamic t (Maybe Int) <- mapDyn _mSelect model
  subPics :: Dynamic t (Map Int BoundingBox) <- mapDyn _mSubPics model
  topSize :: Dynamic t (Int,Int) <- mapDyn _mGeom model

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
  (e,(img,dels,dones,zooms)) <- elDynAttr' "div" subPicAttrs $ do

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
    let -- zooms = fmap (SetZoom . fst . first (/ 200)) $ imageSpaceWheel img
        zooms = fmap (uncurry ZoomAbout) $ imageSpaceWheel img
    return (img,dels, dones, zooms)

  return $ leftmost [SelBox k <$ gate (not <$> current isSel)
                                      (domEvent Click (siEl img))
                    , zooms
                    , traceEvent "Del" dels
                    , traceEvent "DoneClick" dones
                    ]

  where mkSubPicAttrs b = "class" =: "sub-picture-top"
                       <> "style" =: ("pointer-events:none;position:absolute;top:0px;left:0px;"
                                      ++ bool unselstyle selstyle b)
          where unselstyle = "border: 1px solid black;"
                selstyle   = "border: 1px solid black; box-shadow: 0px 0px 10px white;"


imageSpaceToWidgetSpace :: Model -> (Double,Double) -> (Double,Double)
imageSpaceToWidgetSpace m (x,y) =
  let (widNat, heiNat)   = bimap fI fI $ _mNatSize m
      (widGeom,heiGeom)  = bimap fI fI $ _mGeom m
      zm                 = _mZoom m
      scaleCoeff         = widGeom / widNat * _mZoom m
      (focusX,focusY)    = _mFocus m
      -- How far was click from widget corner
      -- click was (x,y) natural pixels from image corner
      -- assuming the image was in the corner of the widget, widget-space click was:
      -- But the image isn't in the widget's corner. The corner is offest
      (xOff,yOff) = (focusX - widNat / 2, focusY - heiNat/2)
      -- (x',y') = (x * widGeom / widNat * zm, y * heiGeom / heiNat * zm)
      -- (x' - xOff, y' - yOff)
      -- So our final coords:
      -- In one step:
      (x',y') = (x * widGeom / widNat * zm - xOff,
                 y * heiGeom / heiNat * zm - yOff)
  in  (x',y')


widgetSpaceToImageSpace :: Model -> (Double,Double) -> (Double,Double)
widgetSpaceToImageSpace m (x',y') =
  let (widNat, heiNat) = bimap fI fI $ _mNatSize m
      (widGeom, heiGeom) = bimap fI fI $ _mGeom m
      zm                 = _mZoom m
      (focusX,focusY)    = _mFocus m
      (xOff,yOff) = (focusX - widNat / 2, focusY - heiNat/2)
      -- We'll just invert imageSpaceToWidgetSpace
  in  ((x'+xOff)*widNat/widGeom/zm,
       (y'+yOff)*heiNat/heiGeom/zm)

-------------------------------------------------------------------------------
applyModelUpdate :: ModelUpdate -> Model -> Model
applyModelUpdate (DelSubPic k  ) m = m & over mSubPics (Map.delete k)
                                         & set  mSelect  Nothing
applyModelUpdate (AddSubPic b  )     m =
  let k = maybe 0 (succ . fst . fst) (Map.maxViewWithKey $ _mSubPics m)
  in m & over mSubPics (Map.insert k b) -- (Just k, Map.insert k b m)
       & set  mSelect (Just k)
applyModelUpdate (ModifyBox k b)     m = error "ModifyBox unimplemented" -- (Just k, Map.insert k b m) -- TODO: Ok? insert, not update?
applyModelUpdate (SelBox k     )     m = m & set mSelect (Just k)
applyModelUpdate (DeselectBoxes)     m = m & set mSelect Nothing
applyModelUpdate (SetZoom mv)        m   = m & set mZoom (max 1 (_mZoom m * (1 + mv)))
applyModelUpdate (SetFocus (x,y))    m   = m & set mFocus (x,y)
applyModelUpdate (SetGeom (wid,hei)) m = let aspect = fI (fst $ _mNatSize m) / fI (snd $ _mNatSize m) :: Double
                                             (wid',hei') = (round (fI hei * aspect), round ((fI wid / aspect)))
                                             geom'@(w,h) = (max wid wid', max hei hei')
                                             foc' = (fI w / 2, fI h / 2)
                                         in  m & set mGeom (max wid wid', max hei hei') & set mFocus foc'
applyModelUpdate (SetNatSize (w,h))  m = let aspect = fI w / fI h :: Double
                                             (gW,gH) = _mGeom m
                                             (gW',gH') = (fI gH * aspect, fI gW / aspect)
                                             geom' = (round (max (fI gW) gW'), round (max (fI gH) gH'))
                                         in m & set mNatSize (w,h) & set mFocus (fI w/2,fI h/2) & set mGeom geom'
applyModelUpdate (ZoomAbout dz (x,y)) m =
      let (natW, natH) = bimap fI fI $ _mNatSize m
          (picW, picH) = bimap fI fI $ _mGeom    m
          (focX, focY) = _mFocus m
          z  = _mZoom m
          cz = 1 + dz                -- zoom coefficient
          z' = z * cz                -- new zoom

          (widgetX,widgetY)   = imageSpaceToWidgetSpace m (x,y)
          (widgetX0,widgetY0) = (picW / 2, picH / 2)
          (wdX,wdY) = (widgetX - widgetX0, widgetY - widgetY0)
          (wdX',wdY') =  (wdX / cz, wdY / cz)
          (widgetX',widgetY') = widgetSpaceToImageSpace m (wdX' + widgetX0, wdY' + widgetY0)
          focus' = (widgetX', widgetY')

          -- dx = (x' - focX) * z * pixW * natW  -- pixel dist from focus x to zoomAbout x
          -- dx' = dx * dz              -- new dist from focus x to zoomAbout x
          -- dy = (y' - focY) * z * pixH * natH
          -- dy' = dy * dz
          -- x   = dx
          -- focus' = (focX + dx * dz, focY + dy * dz)
      in  m {_mFocus = focus', _mZoom = _mZoom m * (1 +z )}


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


testModel :: Model
testModel = Model { _mFocus = (200.0,174.5)
                  , _mZoom = 1.0
                  , _mSelect = Just 0
                  , _mSubPics = Map.fromList [(0,BoundingBox
                                               {bbTopLeft =
                                                Coord { coordX = 100.0
                                                      , coordY = 100.0}
                                               ,bbBotRight =
                                                Coord {coordX = 200.0
                                                      , coordY = 200.0}})
                                             ]
                  , _mGeom = (800,698)
                  , _mNatSize = (400,349)
                  }

