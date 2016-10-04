{-# language CPP #-}
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}

module Common where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad            (join, liftM2)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Bool
import           Data.Default             (Default, def)
import           Data.List                ((\\), foldl')
import           Data.Maybe               (isJust)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import           Reflex.Dom               hiding (restore)

data SelectMe = SelectMe
data DeselectMe = DeselectMe
data DeleteMe   = DeleteMe

-------------------------------------------------------------------------------
listSelfDeleting :: forall t m a b.(MonadWidget t m)
                 => Map Int a -- ^ Initial elements
                 -> Event t a
                    -- ^ Additional elements added one at a time (TODO: Allow adding via map)
                 -> Event t (Maybe Int) -- --^ Set Selection
                 -> (Int -> a -> Event t a -> m (b, Event t DeselectMe, Event t DeleteMe, Event t SelectMe))
                    -- ^ Child builder
                 -> m (Dynamic t (Map Int b), Dynamic t (Maybe Int))
listSelfDeleting initialElems newElems externalSelections mkChild = mdo
  rec children <- listWithKeyShallowDiff initialElems dElems mkChild
      let nextKey = maybe 0 (succ . fst . fst) . Map.maxViewWithKey <$> children 
      let insElems    = attachWith (\k e -> k =: Just e) (current nextKey) newElems
          selNew      = attachWith (const . Just) (current nextKey) newElems
          delElems = ((fmap.fmap) (const Nothing) . switchPromptlyDyn) $
                     (mergeMap . fmap (\(_,_,c,_) -> c)) <$> children
          unselectElems  = ((fmap.fmap) (const Nothing) . switchPromptlyDyn) $
                           (mergeMap . fmap (\(_,b,_,_) -> b)) <$> children
          selectElems    = (fmap (fmap (fst . fst) . Map.minViewWithKey) . switchPromptlyDyn) $
                           (mergeMap . fmap (\(_,_,_,d) -> d)) <$> children -- TODO really ugly
      let unselectSelected = fmapMaybe id $ attachWith (\i m -> case i >>= flip Map.lookup m of
                                                           Just Nothing -> Just Nothing
                                                           _            -> Nothing
                                                       ) (current selection) unselectElems
      let dElems   = leftmost [insElems, delElems]
      selection <- holdDyn Nothing $
        leftmost [selNew, externalSelections, unselectSelected, selectElems ]
  return $ (, selection) ((Map.map (\(a,_,_,_) -> a)) <$> children)
