{-# language LambdaCase          #-}
{-# language OverloadedStrings          #-}
{-# language FlexibleContexts          #-}
{-# language GADTs          #-}
{-# language RankNTypes          #-}
{-# language RecursiveDo         #-}
{-# language ScopedTypeVariables #-}

module Main where

import Control.Applicative (liftA2)
import Control.Lens ((<&>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM2, liftM)
import Data.Bool
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import FacesWidget
import FaceFeatures
import Canvas2D
import Text.Read
import GHCJS.DOM.Types (IsElement)
import qualified GHCJS.DOM.Element as E
import GHCJS.DOM.EventM
import GHCJS.DOM.WheelEvent
import Reflex
import Reflex.Dom
import ScaledImage
-- import Thumbnail hiding (faceWidget, FaceFeature)
import SubPicSelect

main :: IO ()
main = mainWidget run'''

testimg :: T.Text
testimg = "http://web.mit.edu/greghale/Public/frog-1.jpg"

run''' :: forall t m.MonadWidget t m => m ()
run''' = do
  w <- subPicSelect (def { spsc_imgSrc = testimg} ::  SubPicSelectConfig t)
  face <- dyn =<< forDyn (sps_selection w) (\mSelection -> facePreview mSelection (sps_img w) testimg)
  -- face :: Event t (Maybe (FaceAttributes t)) <- dyn $ ((\mSelection -> facePreview mSelection (sps_img w) testimg) <$> sps_selection w :: _)
  display (sps_boxes w)
  return ()

facePreview :: forall t m. MonadWidget t m
            => Maybe (Int,BoundingBox)
            -> ScaledImage t
            -> T.Text
            -> m (Event t (Int, Maybe (Map.Map Int FaceFeature)))
facePreview Nothing   _  _      = return never
facePreview (Just (boxId,bb)) si imgSrc = do
  faceAttrs <- fmap Just $ faceWidget bb si imgSrc
  return $ undefined

-- run' :: MonadWidget t m => m ()
-- run' = do
--   t0 <- liftIO getCurrentTime
--   wid :: Dynamic t (Maybe Double) <- readInput "Width" (Just 800)
--   attrs <- forDyn wid $ \w -> "style" =: ("width:" <> (T.pack . show) (fromMaybe 100 w) <> "px;")
--   content <- fmap fst $ elStopPropagationNS Nothing "div" Wheel $ elDynAttr' "div" attrs $ do
--     tn <- thumbnail (ThumbnailConfig { tcSourceImage = testimg
--                                      , tcAttributes   = attrs}) (faceWidget)
--     return ()
--   return ()

  -- t <- tickLossy 0.015 t0 
  -- dynText =<< holdDyn "Waiting tick" (fmap (show . _tickInfo_n) t)

-- run :: forall t m.MonadWidget t m => m ()
-- run = mdo
--   content <- fmap fst $ elAttr' "div" ("class" =: "content") $ do
--     imgSrc <- stringInput "img  src" "http://cbmm.github.io/images/GitHub.png"

--     scaleInp <- readInput "scale" (Just 1)
--     scale <- foldDyn ($) 1 $ leftmost [fmap const (fmapMaybe id $ updated scaleInp)
--                                       ,wheelScale]
--     bounding <- fmap updated $ readInput "Bounding" (Just $ BoundingBox (Coord 0 0) (Coord 10 10))

--     offset :: Dynamic t (Maybe (Double,Double)) <- liftA2 (,)
--       `mapWidget` readInput "x offset" (Just 0)
--       `apWidget`  readInput "y offset" (Just 0)

--     si <- scaledImage def
--           { sicInitialSource = "http://cbmm.github.io/images/GitHub.png"
--           , sicSetSource = fmapMaybe id $ updated imgSrc
--           , sicSetScale  = updated scale
--           , sicTopLevelAttributes = constDyn ("style" =: "width:200px;")
--           , sicSetOffset = fmapMaybe id $ updated offset
--           , sicInitialBounding = Nothing
--           , sicSetBounding = bounding
--           , sicImgStyle  = constDyn "box-shadow: 10px 10px 10px black;"
--           }
--     -- let clks :: Event t (Int,Int) = domEvent Mousedown (siEl si)
--     --     clkInfo = attachWith ($) (current $ siNaturalCoords si) clks
--     el "br" (return ())
--     dynText =<< holdDyn "No clicks" (fmap show $ leftmost [imageSpaceClick si, imageSpaceMousemove si
--                                                           ,imageSpaceMousedown si, imageSpaceMouseup si])
--     el "hr" (return ())
--     p <- readInput "Test imgSpace" (Just (0,0))
--     dynText =<< combineDyn (\f p -> show $ fmap f p) (screenToImageSpace si) p
--     return ()

--   cWheeled :: Event t Double <- wrapDomEvent (_el_element content) (`on` E.wheel) (mousewheelHandler)
--   let wheelScale = ffor cWheeled $ \n -> bool (* 0.9) (* 1.1) (n > 0)

--   sInfo <- dynText =<< holdDyn "Awaiting scroll" (fmap show $ cWheeled)
--   return ()



mousewheelHandler :: EventM e WheelEvent Double
mousewheelHandler = do
  e <- event
  getDeltaY e

stringInput :: forall t m. MonadWidget t m => T.Text -> T.Text -> m (Dynamic t (Maybe T.Text))
stringInput inputLabel sDef = do
  text inputLabel
  rec -- t <- snd <$> elDynAttr' "div" divAttrs (value <$> textInput def { _textInputConfig_initialValue = sDef })
      t :: Dynamic t T.Text <- fmap value $ elDynAttr "div" divAttrs (textInput def { _textInputConfig_initialValue = sDef })
      let v = (\tx -> bool (Just tx) Nothing (T.null tx)) <$> t
          divAttrs :: Dynamic t (Map.Map T.Text T.Text) = bool mempty ("style" =: "background-color:rgba(255,0,0,0.1);") . T.null <$> t
  return v


readInput :: (Show a, Read a, MonadWidget t m) => T.Text -> Maybe a -> m (Dynamic t (Maybe a))
readInput inputLabel dDef = do
  text inputLabel
  rec t <- snd <$> elDynAttr' "div" divAttrs
                   (value <$> textInput def
                    { _textInputConfig_initialValue = maybe "" (T.pack . show) dDef
                    , _textInputConfig_attributes   = constDyn $ "size" =: "100"
                    })
      let d = readMaybe . T.unpack <$> t
          divAttrs = d <&> \case
            Nothing -> "style" =: "background-color:rgba(255,0,0,0.1);"
            Just _  -> mempty
  return d

mapWidget :: MonadWidget t m => (a -> b) -> m (Dynamic t a) -> m (Dynamic t b)
mapWidget f mw = do
  w <- mw
  return $ f <$> w

apWidget :: MonadWidget t m => m (Dynamic t (a -> b)) -> m (Dynamic t a) -> m (Dynamic t b)
apWidget mf ma = do
  f <- mf
  a <- ma
  return $ zipDynWith ($) f a

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d
