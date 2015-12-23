{-# LANGUAGE RankNTypes, TypeFamilies #-}

module Widgets.Core ( module Widgets.Core
                    , module Widgets.Core.UI
                    , module Widgets.Core.FRP
    ) where

import Widgets.Core.UI
import Widgets.Core.FRP

-- Wrapper for bundling UI elements and their reactive components
class Courier a where
  type Tidal a :: *
  type Element a :: *
  type Temporal a :: *
  tide :: a -> Tidings (Temporal a) (Tidal a)
  element :: a -> Element a
