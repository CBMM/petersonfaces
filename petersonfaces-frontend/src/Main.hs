{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}
{-# language RecursiveDo         #-}
{-# language LambdaCase          #-}

module Main where

import Control.Applicative (liftA2)
import Control.Monad (liftM2, liftM)
import Data.Bool
import Data.Maybe
import Data.Monoid
import FacesWidget
import Canvas2D
import Text.Read
import GHCJS.DOM.Types (IsElement)
import qualified GHCJS.DOM.Element as E
import GHCJS.DOM.EventM
import GHCJS.DOM.WheelEvent
import Reflex
import Reflex.Dom
import ScaledImage
import Thumbnail

main :: IO ()
main = mainWidget run'

testimg   = "https://upload.wikimedia.org/wikipedia/commons/5/55/Atelopus_zeteki1.jpg"
testimg'  = "file:///Users/greghale/Programming/caffe/examples/images/fish-bike.jpg"
testimg'' = "file:///home/greghale/Documents/bicycle.jpg"


run' :: MonadWidget t m => m ()
run' = do
  wid :: Dynamic t (Maybe Double) <- readInput "Width" 800
  attrs <- forDyn wid $ \w -> "style" =: ("width:" <> show (fromMaybe 100 w) <> "px;")
  content <- fmap fst $ elDynAttr' "div" attrs $ do
    tn <- thumbnail def { tcSourceImage = testimg
                        , tcAttributes   = attrs}
    return ()
  return ()

run :: forall t m.MonadWidget t m => m ()
run = mdo
  content <- fmap fst $ elAttr' "div" ("class" =: "content") $ do
    imgSrc <- stringInput "img  src" "http://cbmm.github.io/images/GitHub.png"

    scaleInp <- readInput "scale" 1
    scale <- foldDyn ($) 1 $ leftmost [fmap const (fmapMaybe id $ updated scaleInp)
                                      ,wheelScale]

    offset :: Dynamic t (Maybe (Double,Double)) <- liftA2 (,)
      `mapWidget` readInput "x offset" 0
      `apWidget`  readInput "y offset" 0

    si <- scaledImage def
          { sicInitialSource = "http://cbmm.github.io/images/GitHub.png"
          , sicSetSource = fmapMaybe id $ updated imgSrc
          , sicSetScale  = updated scale
          , sicTopLevelAttributes = constDyn ("style" =: "width:200px;")
          , sicSetOffset = fmapMaybe id $ updated offset
          , sicImgStyle  = constDyn "box-shadow: 10px 10px 10px black;"
          }
    -- let clks :: Event t (Int,Int) = domEvent Mousedown (siEl si)
    --     clkInfo = attachWith ($) (current $ siNaturalCoords si) clks
    el "br" (return ())
    dynText =<< holdDyn "No clicks" (fmap show $ leftmost [imageSpaceClick si, imageSpaceMousemove si
                                                          ,imageSpaceMousedown si, imageSpaceMouseup si])
    return ()

  cWheeled <- wrapDomEvent (_el_element content) (`on` E.wheel) (mousewheelHandler)
  let wheelScale = ffor cWheeled $ \n -> bool (* 0.9) (* 1.1) (n > 0)

  sInfo <- dynText =<< holdDyn "Awaiting scroll" (fmap show $ cWheeled)
  return ()

mousewheelHandler :: EventM e WheelEvent Double
mousewheelHandler = do
  e <- event
  getDeltaY e

stringInput :: MonadWidget t m => String -> String -> m (Dynamic t (Maybe String))
stringInput str sDef = do
  text str
  rec t <- snd <$> elDynAttr' "div" divAttrs (value <$> textInput def { _textInputConfig_initialValue = sDef })
      v <- forDyn t $ \tx -> bool (Just tx) Nothing (null tx)
      divAttrs <- forDyn t $ bool mempty ("style" =: "background-color:rgba(255,0,0,0.1);") . null
  return v


readInput :: (Show a, Read a, MonadWidget t m) => String -> a -> m (Dynamic t (Maybe a))
readInput str dDef = do
  text str
  rec t <- snd <$> elDynAttr' "div" divAttrs (value <$> textInput def { _textInputConfig_initialValue = show dDef })
      d <- mapDyn readMaybe t
      divAttrs <- forDyn d $ \case
        Nothing -> "style" =: "background-color:rgba(255,0,0,0.1);"
        Just _  -> mempty
  return d

mapWidget :: MonadWidget t m => (a -> b) -> m (Dynamic t a) -> m (Dynamic t b)
mapWidget f mw = do
  w <- mw
  mapDyn f w

apWidget :: MonadWidget t m => m (Dynamic t (a -> b)) -> m (Dynamic t a) -> m (Dynamic t b)
apWidget mf ma = do
  f <- mf
  a <- ma
  combineDyn ($) f a

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d
