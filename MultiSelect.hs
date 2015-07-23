{-# LANGUAGE ScopedTypeVariables #-}

module Widgets.MultiSelect where

import Widgets.Core
import Tidings

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import qualified Data.Map as Map
import Data.Map (Map)

-- Widget stuff

type MultiSelect b = MultiListBox b

multiSelect :: forall t a b. (Ord a, Frameworks t)
            => MultiSelect b
            -> Behavior t [a]
            -> Behavior t [a]
            -> Behavior t (a -> String)
            -> Moment t (Tidings t [a])
multiSelect multi bitems bsels bdisplay = do
    -- animate output items
    sink multi [ items :== map <$> bdisplay <*> bitems ]

    -- animate output selection
    let bindices :: Behavior t (Map a Int)
        bindices = indexify bitems
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
    let bindices2 :: Behavior t (Map Int a)
        bindices2 = Map.fromList . zip [0..] <$> bitems
    esel <- eventSelections multi
    return $ tidings bsels $ lookupIndices <$> bindices2 <@> esel

eventSelections :: forall b t. Frameworks t => MultiListBox b -> Moment t (Event t [Int])
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
