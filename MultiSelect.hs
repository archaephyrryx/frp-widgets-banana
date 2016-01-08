{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module Widgets.MultiSelect where

import Widgets.Core

import qualified Data.Map as Map
import Data.Map (Map)

-- Widget stuff

type MultiSel  = forall a. MultiListBox a
type MultiSel  =      MultiListBox ()

data MultiSelect a =
     MultiSelect { _list :: MultiSel
                 , _selections :: Tidings [a]
                 }

multiSelect :: Ord a
            => MultiSel
            -> Behavior [a]
            -> Behavior [a]
            -> Behavior (a -> RenderedValue)
            -> MomentIO (MultiSelect a)
multiSelect multi bitems bsels bdisplay = do
    -- animate output items
    sink multi [ items :== map <$> bdisplay <*> bitems ]

    -- animate output selection
    let bindices = indexify bitems
        indexify = ((Map.fromList . flip zip [0..]) <$>)
        bsindices   = lookupIndices <$> bindices <*> bsels

        lookupIndices indices [] = []
        lookupIndices indices (sel:selt) = let rest = lookupIndices indices selt
                                           in maybe rest (:rest) (Map.lookup sel indices)

    sink multi [ selections :== bsindices ]

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
    let bindices2 = Map.fromList . zip [0..] <$> bitems
    esel <- eventSelections multi
    let selects = tidings bsels $ lookupIndices <$> bindices2 <@> esel
    return $ MultiSelect multi selects

eventSelections :: MultiListBox b -> MomentIO (Event [Int])
eventSelections w = do
    liftIO $ fixSelectionsEvent w
    addHandler <- liftIO $ event1ToAddHandler w (event0ToEvent1 select)
    fromAddHandler $ mapIO (const $ get w selections) addHandler

fixSelectionsEvent listbox =
    liftIO $ set listbox [ on unclick := handler ]
    where
    handler _ = do
          propagateEvent
          s <- get listbox selections
          when (s == []) $ (get listbox (on select)) >>= id
