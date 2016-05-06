{-# language DeriveGeneric #-}

module Face where

import GHC.Generics

data FaceLoc = FaceLoc
  { faceCenterX :: Double
  , faceCenterY :: Double
  , faceWidth   :: Double
  , faceHeight  :: Double
  } deriving (Eq, Ord, Show, Generic)

