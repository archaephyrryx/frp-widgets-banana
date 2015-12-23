{-# LANGUAGE RecursiveDo, RecordWildCards #-}

module Widgets.Obscura where

import Widgets.Core
import Widgets.Links

obscura :: Behavior (a -> String) -- Image URL to display
        -> Behavior a -- Value to hold
        -> UI (LiquidLink a)
obscura bCurler fluid = do
    link <- image #. "obscura" # set forbidContext ()
    element link # sink src (bCurler <*> fluid)

    let _elementLL = link
        _fluxLL = fluid
    return LiquidLink{..}
