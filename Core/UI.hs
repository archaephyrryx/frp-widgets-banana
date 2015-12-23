{-# LANGUAGE RankNTypes, TypeFamilies, TypeSynonymInstances,
    FlexibleInstances #-}

module Widgets.Core.UI ( module Widgets.Core.UI
                       , module Graphics.UI.WX
                       ) where

import Graphics.UI.WX hiding (Event, newEvent, empty, Identity)

-- As we don't have UI Element, we are using string here temporarily to
-- represent the rendered items on the list
type RenderedValue = String

type ChildValue = Window ()

class (Show a) => Stringent a where
    stringify :: a -> String

instance Stringent String where
    stringify = id

renderValue :: Stringent a => a -> RenderedValue
renderValue = stringify

renderChild :: Window a -> RenderedValue -> IO ChildValue
renderChild w s = do
      f <- window w []
      x <- staticText f [ text := s ]
      return f

