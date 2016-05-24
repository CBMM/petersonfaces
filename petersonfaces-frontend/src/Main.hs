{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}

module Main where

import FacesWidget
import Canvas2D
import Reflex
import Reflex.Dom
import Thumbnail

main :: IO ()
main = mainWidget run

run :: forall t m.MonadWidget t m => m ()
run = do
  t  <- fmap value $ textInput def
  el "br" (return ())
  elAttr "span" ("style" =: "width:100px;height:100px;background-color:blue;") (text "span")
  si <- scaledImage (ScaledImageConfig t (constDyn mempty))
  let clks :: Event t (Int,Int) = domEvent Mousedown (siEl si)
      clkInfo = attachWith ($) (current $ siNaturalCoords si) clks
  el "br" (return ())
  dynText =<< holdDyn "No clicks" (fmap show clkInfo)
  return ()
