{-# LANGUAGE ScopedTypeVariables #-}

module Widgets.MonoSelect where


import Widgets.Core

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

type MonoSelect b = SingleListBox b

monoSelect :: forall a b t. (Ord a, Frameworks t)
  => MonoSelect b
  -> Behavior t [a] -- ^ List of values
  -> Behavior t [a] -- ^ Selected value (singleton or empty)
  -> Behavior t (a -> RenderedValue) -- ^ Display for regular values
  -> Moment t (Tidings t [a])
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
    return $ tidings bsel $
            lookupIndex <$> bindices2 <@> (one <$> esel)
