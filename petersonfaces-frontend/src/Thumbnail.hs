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
{-# language FlexibleContexts #-}
{-# language RecursiveDo #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}

module Thumbnail where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad            (liftM2)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Bool
import           Data.Default             (Default, def)
import           Data.List                ((\\))
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
import qualified GHCJS.DOM.Types          as GT
import           GHCJS.DOM.WheelEvent     as WheelEvent
import qualified GHCJS.DOM.Element        as E
import           ScaledImage


data ThumbnailConfig t = ThumbnailConfig
  { tcSourceImage :: T.Text
  , tcAttributes  :: Dynamic t (Map T.Text T.Text)
  -- , tcZoom        :: Event t BoundingBox
  -- , tcBoundings   :: Event t (Int, Maybe BoundingBox)
  }

instance Reflex t => Default (ThumbnailConfig t) where
  def = ThumbnailConfig "" (constDyn mempty)

data Thumbnail t a = Thumbnail
  { tElement          :: El t
  , tBoxes            :: Dynamic t (Map Int a)
  , tSelection        :: Dynamic t (Maybe (Int, a))
  , tImageNaturalSize :: Dynamic t (Int,Int)
  , tBigPicture       :: ScaledImage t
  }


data ModelUpdate =
    DelSubPic Int
  | AddSubPic Int ChildConfig
  | ModifyBox Int
  | SelBox Int
  | DeselectBoxes
  | SetZoom Double
  | SetFocus (Double, Double)
  | ZoomAbout Double (Double, Double)
  | SetGeom (Int,Int)
  | SetNatSize (Int,Int)
  deriving (Eq, Show)

data ChildConfig = ChildConfig { ccBounding :: BoundingBox }
                 deriving (Eq, Ord, Show)

data Model a = Model
  { _mFocus   :: (Double,Double)
  , _mZoom    :: Double
  , _mSelect  :: Maybe Int
  , _mSubPics :: Map Int ChildConfig
  , _mGeom    :: (Int,Int)
  , _mNatSize :: (Int,Int)
  } deriving (Eq, Show)

model0 :: Model a
model0 = Model (0,0) 1 Nothing mempty (0,0) (0,0)

makeLenses ''Model

type ThumbnailChild t m a =
     Event   t (Double,Double)
  -> Event   t ()
  -> Dynamic t ((Double,Double) -> (Double,Double))
  -> Dynamic t ((Double,Double) -> (Double,Double))
  -> Dynamic t (Int,Int) -- ^ Natural size of base image
  -> Dynamic t Double    -- ^ Zoom
  -> Dynamic t (Double,Double) -- ^ Focus point
  -> Dynamic t (Maybe Int)  -- ^ Selection index
  -> Dynamic t Double  -- ^ Top-level scale
  -> Int
  -> ChildConfig -> Event t ChildConfig -> m a



-- | A complicated widget used for panning, zooming, and region-selecting within an img
--   Movement is achieved through clicks and mousewheels in a 'picture-in-picture' view
--   in the corner of the widget
thumbnail :: forall t m a.MonadWidget t m
          => ThumbnailConfig t
          -> (ThumbnailChild t m a)
          -> m (Thumbnail t a)
thumbnail (ThumbnailConfig srcImg attrs) mkChild = mdo
  pb <- getPostBuild

  let topAttrs :: Dynamic t (Map T.Text T.Text) = zipDynWith (Map.unionWith (<>))
        (constDyn $ "class" =: "thumbnail-widget "
         <> "style" =: "position:relative; -webkit-user-select: none; user-select: none;")
        attrs

  firstLoad <- headE $ domEvent Load $ siImgEl $ tBigPicture tn

  let natSizes = SetNatSize <$> uniqDyn (tImageNaturalSize tn)

  topResizes <- (fmap . fmap) SetGeom $
    (performEvent (ffor (leftmost [pb, () <$ updated attrs]) $ \() -> do
                      Just r <- getBoundingClientRect $ _element_raw tnWidget
                      liftM2 (,) (floor <$> getWidth r) (floor <$> getHeight r)))


  let outerScale = zipDynWith (\(natWid,natHei) (wWid,wHei) -> case (wWid,wHei) of
                               (0,0) -> 1
                               _     -> if   wWid == 0
                                        then fI wHei / fI natHei
                                        else fI wWid / fI natWid)
                (tImageNaturalSize tn) topSize

  -- (resize,(tnWidget, (tn,model))) <- resizeDetectorWithStyle
  --  "width:100%;height:100%;" $
  --  elDynAttr' "div" topAttrs $ elStopPropagationNS Nothing "div" Wheel $
  (tnWidget, (tn,model)) <- elDynAttr' "div" topAttrs $
   mdo


    let thumbPosition = ffor (updated $ siNaturalSize bigPic) $ \(natW, natH) ->
          (0.9 * fI natW :: Double, 0 :: Double)

    let natSize = _mNatSize <$> model
        zoom    = _mZoom  <$> model
        focus   = _mFocus <$> model
        zoomPos = model <&> \m -> (_mZoom m, _mFocus m) -- TODO cleanup

    let bigPicAttrs = sel <&> \case
          Nothing -> "class" =: "big-picture" <> "style" =: "position:absolute"
          Just i  -> "class" =: "big-picture bp-darkened"
                     <> "style" =:
                     ("position:absolute;filter: blur(2px) brightness(90%);"
                      <> " -webkit-filter: blur(2px) brightness(90%); opacity:0.5;")

    let setOffsets = fmap modelOffset (updated model)
    bigPic   <- elDynAttr "div" bigPicAttrs $
      scaledImage def
        { sicInitialSource      = srcImg
        , sicCroppingAttributes = bigPicAttrs
        , sicTopLevelScale      = outerScale
        , sicSetOffset          = setOffsets
        , sicSetScale           = updated zoom
        }

    -- performEvent $ fmap (liftIO . print) (imageSpaceClick bigPic)
    -- performEvent $ ffor (attach (current model) $ imageSpaceClick bigPic) $ \(m, (x,y)) ->
    --   liftIO (putStrLn $ "widgetspace: " ++ show (imageSpaceToWidgetSpace m (x,y)))

    dblClicks <- wrapDomEvent (_element_raw (siEl bigPic)) (`on` E.dblClick) getMouseEventCoords
    let okToAddSelection = fmap (== Nothing) (current sel)
        newSelectionPos (z,(wid,hei),k) (x,y) =
            let (boxWid,boxHei) = (fI wid / z / 4, fI hei / z / 4)
                topLeft  = Coord (max 0 $ x - boxWid/2)            (max 0 $ y - boxHei/2)
                botRight = Coord (min (fI wid - 1) $ x + boxWid/2) (min (fI hei - 1) $ y + boxHei/2)
            in  AddSubPic k (ChildConfig $ BoundingBox topLeft botRight)
        addSel = gate okToAddSelection $
                 attachWith newSelectionPos
                            ((,,) <$> current zoom
                                  <*> current (siNaturalSize bigPic)
                                  <*> fmap (nextKey . _mSubPics) (current model))
                            (attachWith ($) (current $ screenToImageSpace bigPic) (fmap (bimap fI fI) dblClicks))

    model :: Dynamic t (Model a) <- foldDyn applyModelUpdate model0
        (traceEvent "" $ leftmost [fmap SetFocus thumbPosUpdates
                                        ,updated natSizes
                                        -- ,fmap snd subPicEvents
                                        -- ,AddSubPic testBox <$ pb'
                                        , topResizes
                                        -- , addSel
                                        , zooms
                                        ])

    let mousemoves = fmap (bimap fI fI) $ domEvent Mousemove (siEl bigPic)
        dragEnds   = leftmost [() <$ domEvent Mouseup (siEl bigPic)]
        selKey = _mSelect <$> model
        topScale = model <&> (\m -> fI (fst (_mGeom m)) /
                                    fI (fst (_mNatSize m)))
    let newChild = ffor addSel $ \(AddSubPic k cfg) -> k =: Just cfg
    subPics <- listWithKeyShallowDiff
      mempty newChild (mkChild mousemoves dragEnds (screenToImageSpace bigPic)
                       (imageToScreenSpace bigPic)
                       natSize zoom focus selKey topScale)

    let thumbScale = (/4) <$> outerScale
    thumbPic :: ScaledImage t <- elAttr "div"
      ("style" =: "position:absolute;opacity:0.5;top:0px;" <>
       "class" =: "thumbnail-navigator") $
      scaledImage def { sicInitialSource = srcImg
                      , sicInitialScale  = 1
                      , sicTopLevelScale = thumbScale
                      }

    wheels <- wheelEvents (siEl bigPic)
    let thumbPosUpdates = attachWith ($) (current $ screenToImageSpace thumbPic) (bimap fI fI <$> domEvent Mousedown (siEl thumbPic))
        zooms           = attachWith (\f (dz, pnt) -> ZoomAbout (dz/200) (f pnt)) (current $ screenToImageSpace bigPic) wheels
        -- zooms           = fmap (\(dz,pnt) -> ZoomAbout (dz/200) pnt)
        --   (imageSpaceWheel bigPic)

    return $ (Thumbnail undefined undefined undefined (siNaturalSize  bigPic) bigPic, model)

  let sel :: Dynamic t (Maybe Int) = _mSelect <$> model

  -- subPics :: Dynamic t (Map Int a) <- mapDyn _mSubPics model
      topSize :: Dynamic t (Int,Int) = _mGeom <$> model

  return tn


uncenteredOffsets :: Reflex t => ScaledImage t -> Event t (Double, Double) -> Event t (Double, Double)
uncenteredOffsets bigPic thumbPosUpdates =
  ffor (attach (current $ siNaturalSize bigPic) thumbPosUpdates) $ \((w,h),(x,y)) ->
  (fI w/2 - x, fI h/2 - y)

modelOffset :: Model a -> (Double, Double)
modelOffset m = getOffset (_mNatSize m) (_mFocus m) (_mZoom m)

getOffset :: (Int,Int) -> (Double,Double) -> Double -> (Double,Double)
getOffset (natW,natH) (focX,focY) zoom = (fI natW/2/zoom - focX, fI natH/2/zoom - focY)

imageToWidget :: (Int, Int) -> (Double,Double) -> Double -> (Double, Double) -> (Double, Double)
imageToWidget natGeom (focX, focY) s (x,y) =
  let (offX,offY) = getOffset natGeom (focX,focY) s
  in ((x - offX)/s, (y - offY)/s)

widgetToImage :: (Int,Int) -> (Double,Double) -> Double -> (Double,Double) -> (Double,Double)
widgetToImage natGeom (focX,focY) s (x,y) =
  let (offX,offY) = getOffset natGeom (focX,focY) s
  in (s*x + offX, s*y + offY)

type FaceFeature = (T.Text, (Double,Double))
type FaceUpdate = Map Int (Maybe FaceFeature)
data Face = Face { faceFeatures ::  Map Int FaceFeature }


-- TODO Use the one in FaceFeatures.hs if anything
--      Maybe difficult since faceWidget here doesn't handle its own events.
faceWidget :: forall t m.MonadWidget t m
           -- => Dynamic t (Int,Int)
           => Event t (Double,Double)
           -> Event t ()
           -> Dynamic t ((Double,Double) -> (Double,Double))
           -> Dynamic t ((Double,Double) -> (Double,Double))
           -> Dynamic t (Int,Int) -- ^ Parent image natural size
           -> Dynamic t Double
           -> Dynamic t (Double,Double)
           -> Dynamic t (Maybe Int)
           -> Dynamic t Double
           -> Int
           -> ChildConfig
           -> Event t ChildConfig
           -> m (Dynamic t Face)
faceWidget mouseMoves dragEnds imgToWidg widgToImg natSize zoom focus selKey topScale k cfg0 dCfg = mdo
  pb <- getPostBuild
  rect <- fmap ccBounding <$> holdDyn cfg0 dCfg
  let relativeFeatures natGeom foc z =
        let ChildConfig (BoundingBox (Coord bX0 bY0) (Coord bX1 bY1)) = cfg0
            (wid,hei) = (bX1 - bX0, bY1 - bY0)
        in   Map.fromList [(0,("Eye",   (bX0 + wid/2 - 50, bY0 + hei/2 - 20)))
                          ,(1,("Eye",   (bX0 + wid/2 + 50,  bY0 + hei/2 - 20)))
                          ,(2,("Nose",  (bX0 + wid/2, bY0 + hei/2)))
                          ,(3,("Mouth", (bX0 + wid/2, bY0 + hei/2 + 50)))
                          ]

      geomInfo = (,,) <$> current natSize <*> current focus <*> current zoom
      defFeatures :: Event t FaceUpdate = ffor (tag geomInfo pb) $ \(s,f,z) ->
        fmap Just (relativeFeatures s f z)
      faceUpdates = leftmost [defFeatures, faceDeletions]

  facePartEvents <-listWithKeyShallowDiff mempty faceUpdates (faceFeatureWidget mouseMoves dragEnds imgToWidg widgToImg rect)
  let faceParts :: Dynamic t (Map Int FaceFeature) = joinDynThroughMap $ (fmap fst) <$> facePartEvents

      faceDeletions = ((fmap . fmap) (const Nothing) . switchPromptlyDyn) $
        (mergeMap . fmap snd) <$> facePartEvents
  return $ Face <$> faceParts


faceFeatureWidget :: forall t m. MonadWidget t m
                  => Event t (Double,Double)
                  -> Event t ()
                  -> Dynamic t ((Double,Double) -> (Double,Double))
                  -> Dynamic t ((Double,Double) -> (Double,Double))
                  -> Dynamic t BoundingBox
                  -> Int
                  -> FaceFeature
                  -> Event t FaceFeature
                  -> m (Dynamic t FaceFeature, Event t ())
faceFeatureWidget externalMoves externalEnds toImg fmImg bounding k f0 dF = mdo
  pb <- getPostBuild
  -- ff :: Dynamic t FaceFeature <- fmap (traceDyn "TEST") $ holdDyn f0 $ leftmost [ dF, fUpdates ]
  ff :: Dynamic t FaceFeature <- holdDyn f0 $ fUpdates -- leftmost [ traceEvent "dF" dF, fUpdates ]

  let (divWid,divHei) :: (Double,Double) = (20,20)
      featureAttrs :: Dynamic t (Map T.Text T.Text) = zipDynWith
        (\f (nm,(x,y)) ->
           let (wX,wY) = f (x,y)
               xOff = "left: " <> T.pack (show (wX - divWid/2)) <> "px;"
               yOff = "top: " <> T.pack (show (wY - divHei/2)) <> "px;"
               wid  = "width: " <> T.pack (show divWid) <> "px;"
               hei  = "height: " <> T.pack (show divHei) <> "px;"

               styl = "position: absolute; border: 1px solid rgba(0,0,0,0.5); background-color: hsla(0,100%,100%,0.25); border-radius:200px;"
           in  "style" =: ( xOff <> yOff <> wid <> hei <> styl) <> "class" =: "face-feature"
        ) fmImg ff
  (ffDiv, dels) <- elDynAttr' "div" featureAttrs $
   elAttr "div" ("class" =: "face-feature-container" <> "style" =: "position:absolute;") $ do
    elAttr "div" ("class" =: "face-feature-dot"
                 <> "style" =: ("position:absolute; top: " <>
                                (T.pack . show) (divHei/2 - 2) <> "px; left: " <>
                                (T.pack . show) (divWid/2 - 2) <> "px; width:4px; height:4px;" <>
                                "background-color:black;border-radius:5px;")) (return ())
    closes <- elAttr "div" ("class" =: "face-feature-info" <> "style" =: "position: absolute; left: 20px;top:-20px;") $
      elAttr "div" ("class" =: "face-feature-delete" <> "style" =:
                    "position:absolute; background-color:rgba(255,255,255,0.5); border-radius:2px; padding: 1px; ") $ do
        text (fst f0)
        fmap (domEvent Click . fst) $
          elAttr' "span" ("class" =: "button" <> "style" =:
                          "color:hsla(3,50%,50%,1); text-shadow:1px 1px rgba(0,0,0,0.5);border-left: 1px solid gray; margin-left: 3px;") $ text "✗"
    return closes

  -- let wrapDrag e = wrapDomEvent (_el_element ffDiv) (`on` e) $ do
  --       ev <- event
  --       (,) <$> liftIO (getClientX ev) <*> liftIO (getClientY ev)
  let starts = fmap (bimap fI fI) (domEvent Mousedown ffDiv)
      ends   = leftmost [externalEnds, () <$ domEvent Mouseup ffDiv]
      moves  = leftmost [fmap (bimap fI fI) (domEvent Mousemove ffDiv), externalMoves]

  draggingOrigin :: Dynamic t (Maybe (FaceFeature, (Double,Double))) <- holdDyn Nothing $ leftmost
    [ fmap Just (attach (current ff) (imgSpace starts))
    -- , fmap Just (attach (current ff) (imgSpace (domEvent Mousedown ffDiv)))
    , Nothing <$ ends
    ] -- domEvent Mouseup ffDiv]
  let isDragging = isJust <$> draggingOrigin
  let imgSpace es = attachWith ($) (current toImg) es
      fUpdates :: Event t FaceFeature
      fUpdates = fmapMaybe id (attachWith (\orig (x,y) -> case orig of
                                              Nothing      -> Nothing
                                              Just ((nm,(oldX,oldY)), (x0,y0)) -> Just (nm, (oldX + x - x0, oldY + y - y0))

                                          ) (current draggingOrigin) (imgSpace moves)) -- (imgSpace $ domEvent Mousemove ffDiv))

  return (ff, dels) -- TODO: Catch delete events




-------------------------------------------------------------------------------
applyModelUpdate :: ModelUpdate -> Model a -> Model a
applyModelUpdate (DelSubPic k  ) m = m -- & over mSubPics (Map.delete k)
                                       & set  mSelect  Nothing
applyModelUpdate (AddSubPic k cfg ) m = m & over mSubPics (Map.insert k cfg)
  -- let k = maybe 0 (succ . fst . fst) (Map.maxViewWithKey $ _mSubPics m)
  -- in m -- & over mSubPics (Map.insert k b)
  --      & set  mSelect (Just k)
applyModelUpdate (ModifyBox k)     m = error "ModifyBox unimplemented" -- (Just k, Map.insert k b m) -- TODO: Ok? insert, not update?
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
  let (focX, focY) = _mFocus m
      cz     = 1 + dz
      z'     = _mZoom m * cz
      focus' = (zoomAbout1D dz focX x, zoomAbout1D dz focY y)
  in  m {_mFocus = focus', _mZoom = z'}

zoomAbout1D :: Double -> Double -> Double -> Double
zoomAbout1D dZoom focus pivotX =
  let pivotDist'    = (pivotX - focus) / (1 + dZoom)
  in  pivotX - pivotDist'


selectMayViewListWithKey :: forall t m k v a. (MonadWidget t m, Ord k)
                         => Dynamic t (Maybe k)
                         -> Dynamic t (Map k v)
                         -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a))
                         -> m (Event t (k,a))
selectMayViewListWithKey sel vals mkChild = do
  let selectionDemux = demux sel
  children <- listWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux (Just k)
    selfEvents <- mkChild k v selected
    return $ fmap ((,) k) selfEvents
  return $ switchPromptlyDyn $ (leftmost . Map.elems) <$> children

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


testModel :: Model BoundingBox
testModel = Model { _mFocus = (200.0,174.5)
                  , _mZoom = 1.0
                  , _mSelect = Just 0
                  , _mSubPics = mempty
                  , _mGeom = (800,698)
                  , _mNatSize = (400,349)
                  }


nextKey :: Map Int v -> Int
nextKey m = head ([0..length m] \\ Map.keys m)


-- subPicture :: MonadWidget t m
--            => String -- ^ image src
--            -> Dynamic t (Int,Int) -- ^ Parent image natural size
--            -> Dynamic t Double -- ^ zoom
--            -> Dynamic t (Double,Double) -- ^ focus point
--            -> Dynamic t (Maybe Int) -- ^ Selection key?
--            -> Dynamic t Double -- ^ Top level (whole-widget) extra scaling
--            -> Int -- ^ Key
--            -> ChildConfig
--            -> Event t ChildConfig
--            -> m (Event t ModelUpdate)
-- subPicture srcImg natSize zoom focus selKey topScale k cfg0 dCfg = mdo
--   pb <- getPostBuild

--   isSel <- mapDyn (== Just k) selKey
--   rect <- mapDyn ccBounding =<< holdDyn cfg0 dCfg
--   setOffsets' <- getOffset `mapDyn` natSize `apDyn` focus `apDyn` zoom
--   subPicAttrs <- mkSubPicAttrs `mapDyn` isSel
--   (e,(img,dels,dones,zooms)) <- elDynAttr' "div" subPicAttrs $ do

--     img <- scaledImage def
--            { sicInitialSource   = srcImg
--            , sicTopLevelScale   = topScale
--            , sicSetScale        = updated zoom
--            , sicSetOffset       = updated setOffsets'
--            , sicSetBounding     = fmap Just . leftmost $ [tag (current rect) pb, updated rect]
--            }
--     dels  <- fmap (DelSubPic k <$)
--            (elAttr "div" ("style" =: "pointer-events:auto;") $ button "x")
--     dones <- fmap (DeselectBoxes <$) (elAttr "div" ("style" =: "pointer-events:auto;") $ button "o")
--     let -- zooms = fmap (SetZoom . fst . first (/ 200)) $ imageSpaceWheel img
--         zooms = fmap (uncurry ZoomAbout) $ imageSpaceWheel img
--     return (img,dels, dones, zooms)

--   return $ leftmost [SelBox k <$ gate (not <$> current isSel)
--                                       (domEvent Click (siEl img))
--                     , dels
--                     , dones
--                     ]

--   where mkSubPicAttrs b = "class" =: "sub-picture-top"
--                        <> "style" =: ("pointer-events:none;position:absolute;top:0px;left:0px;"
--                                       ++ bool unselstyle selstyle b)
--           where unselstyle = "border: 1px solid black;"
--                 selstyle   = "border: 1px solid black; box-shadow: 0px 0px 10px white;"


-- imageSpaceToWidgetSpace :: Model a -> (Double,Double) -> (Double,Double)
-- imageSpaceToWidgetSpace m (x,y) =
--   let (widNat, heiNat)   = bimap fI fI $ _mNatSize m
--       (widGeom,heiGeom)  = bimap fI fI $ _mGeom m
--       zm                 = _mZoom m
--       scaleCoeff         = widGeom / widNat * _mZoom m
--       (focusX,focusY)    = _mFocus m
--       (xOff,yOff) = (focusX - widNat / 2, focusY - heiNat/2)
--       (x',y') = (x * widGeom / widNat * zm - xOff,
--                  y * heiGeom / heiNat * zm - yOff)
--   in  (x',y')


-- widgetSpaceToImageSpace :: Model a -> (Double,Double) -> (Double,Double)
-- widgetSpaceToImageSpace m (x',y') =
--   let (widNat, heiNat) = bimap fI fI $ _mNatSize m
--       (widGeom, heiGeom) = bimap fI fI $ _mGeom m
--       zm                 = _mZoom m
--       (focusX,focusY)    = _mFocus m
--       (xOff,yOff) = (focusX - widNat / 2, focusY - heiNat/2)
--       -- We'll just invert imageSpaceToWidgetSpace
--   in  ((x'+xOff)*widNat/widGeom/zm,
--        (y'+yOff)*heiNat/heiGeom/zm)
