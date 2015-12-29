{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}

module Widgets.Core ( module Widgets.Core
                    , module Widgets.Core.UI
                    , module Widgets.Core.FRP
    ) where

import Widgets.Core.UI
import Widgets.Core.FRP

-- Wrapper for bundling UI elements and their reactive components
class Courier c t | c -> t where
  type Element c :: *
  tide :: c -> Tidings t
  element :: c -> Element c
