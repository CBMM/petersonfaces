{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}
{-# language RecursiveDo         #-}
{-# language LambdaCase          #-}

module Main where

import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Data.Bool
import FacesWidget
import Canvas2D
import Text.Read
import Reflex
import Reflex.Dom
import Thumbnail

main :: IO ()
main = mainWidget run

run :: forall t m.MonadWidget t m => m ()
run = do
  imgSrc <- stringInput "img  src"

  scale <- doubleInput "scale"

  offset :: Dynamic t (Maybe (Double,Double)) <- liftA2 (,)
    `mapWidget` doubleInput "x offset"
    `apWidget`  doubleInput "y offset"

  si <- scaledImage def { sicSetSource = fmapMaybe id $ updated imgSrc
                        , sicSetScale  = fmapMaybe id $ updated scale
                        , sicSetOffset = fmapMaybe id $ updated offset}
  -- let clks :: Event t (Int,Int) = domEvent Mousedown (siEl si)
  --     clkInfo = attachWith ($) (current $ siNaturalCoords si) clks
  el "br" (return ())
  -- dynText =<< holdDyn "No clicks" (fmap show clkInfo)
  return ()

stringInput :: MonadWidget t m => String -> m (Dynamic t (Maybe String))
stringInput str = do
  text str
  rec t <- snd <$> elDynAttr' "div" divAttrs (value <$> textInput def)
      v <- forDyn t $ \tx -> bool (Just tx) Nothing (null tx)
      divAttrs <- forDyn t $ bool mempty ("style" =: "background-color:rgba(255,0,0,0.1);") . null
  return v


doubleInput :: MonadWidget t m => String -> m (Dynamic t (Maybe Double))
doubleInput str = do
  text str
  rec t <- snd <$> elDynAttr' "div" divAttrs (value <$> textInput def)
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
