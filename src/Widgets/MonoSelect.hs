{-# LANGUAGE ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Widgets.MonoSelect where


import Widgets.Core

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Util

{-
type MonoSel = forall a. SingleListBox a
---}
--{-
type MonoSel = SingleListBox ()
--}


data MonoSelect a = MonoSelect
                  { _list :: MonoSel
                  , _selection :: Tidings [a]
                  }

instance Courier (MonoSelect a) [a] where
  type Element (MonoSelect a) = MonoSel
  tide = _selection
  element = _list


monoSelect :: Ord a
  => MonoSel
  -> Behavior [a] -- ^ List of values
  -> Behavior [a] -- ^ Selected value (singleton or empty)
  -> Behavior (a -> RenderedValue) -- ^ Display for regular values
  -> MomentIO (MonoSelect a)
monoSelect mono bitems bsel bdisplay = do
    sink mono [ items :== map <$> bdisplay <*> bitems ]

    let bindices = indexify bitems
        indexify = (Map.fromList . flip zip [0..] <$>)
        bsindex = lookupIndex <$> bindices <*> bsel

        lookupIndex indices [] = []
        lookupIndex indices (sel:_) = maybeToList $ Map.lookup sel indices

        selectone = mapAttr one (const $ cond full head $ const (-1)) selection

    sink mono [ selectone :== bsindex ]

    let bindices2 = Map.fromList . zip [0..] <$> bitems
    esel <- eventSelection mono
    let
      selection = tidings bsel $ lookupIndex <$> bindices2 <@> (one <$> esel)
    return $ MonoSelect mono selection
