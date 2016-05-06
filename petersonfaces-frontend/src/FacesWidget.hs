{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language DeriveGeneric #-}

module FacesWidget where

-------------------------------------------------------------------------------
import           Data.Default
import           Reflex
import           Reflex.Dom
import qualified Data.Map as Mapp
import           Data.Map (Map)
-------------------------------------------------------------------------------
import           Face


-------------------------------------------------------------------------------
data FacesWidgetConfig t = FacesWidgetConfig
  { facesWidgetConfig_attributes    :: Dynamic t (Map String String)
  , facesWidgetConfig_initialFaces  :: Map Int FaceLoc
  , facesWidgetConfig_setFace       :: Event t (Int, Maybe FaceLoc)
  , facesWidgetConfig_intialPic     :: PicUrl
  , facesWidgetConfig_setPic        :: Event t PicUrl
  , facesWidgetConfig_select        :: Event t Int
  , facesWidgetConfig_initialOffset :: (Double,Double)
  , facesWidgetConfig_offset        :: Event t (Double,Double)
  , facesWidgetConfig_initialZoon   :: Double
  , facesWidgetConfig_zoom          :: Event t Double
  }

data FacesWidget t = FacesWidget
  { facesWidget_faces  :: Dynamic t (Map Int FaceLoc)
  , facesWidget_canvas :: El t
  , facesWidget_selection :: Dynamic t (Maybe (Int, FaceLoc))
  }

instance Default (FacesWidgetConfig t) where
  def = FacesWidgetConfig (constDyn mempty) mempty
                          never (PicUrl "") never never
                          (0,0) never 1 never


facesWidget :: forall t m.MonadWidget t m => FacesWidgetConfig t -> m (FacesWidget t)
facesWidget cfg = elClass "div" "faces-widget" $ do

  canv <- 
