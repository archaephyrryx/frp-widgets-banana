{-# LANGUAGE RankNTypes, TypeFamilies, TypeSynonymInstances,
    FlexibleInstances #-}

module Widgets.Core.UI ( module Widgets.Core.UI
                       , module Graphics.UI.WX
                       , module Graphics.UI.WXCore
                       ) where

import Graphics.UI.WXCore hiding (Event, Timer, empty, Identity, newEvent)
import Graphics.UI.WX.Attributes
import Graphics.UI.WX hiding (Event, newEvent, empty, Identity)
import Util (one)
import Data.Stringent

enboxed :: Layout -> Layout
enboxed l = column m [ hrule w, row m [ vrule w, l, vrule w], hrule w]
  where
      m = 5
      w = 2

-- As we don't have UI Element, we are using string here temporarily to
-- represent the rendered items on the list
type RenderedValue = String

type ChildValue = Window ()

renderValue :: Stringent a => a -> RenderedValue
renderValue = stringify

renderChild :: Window a -> RenderedValue -> IO ChildValue
renderChild w s = do
      f <- window w []
      x <- staticText f [ text := s ]
      return f
