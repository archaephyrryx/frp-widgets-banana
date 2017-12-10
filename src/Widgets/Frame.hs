{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GADTs                 #-}
module Widgets.Frame where

import Widgets.Core
import Widgets.Links

type (Internal m) a = m

data GenItem a where
  GenItem :: (Typeable (w a), Widget (w a), Visible (w a)) => w a -> GenItem a

align :: Frame a -> Row
align = map (\(GenItem x) -> (Item x)) . _tiles

data Frame a = Frame { _tiles :: [GenItem a]
                     , _subject :: Behavior a
                     }
                     deriving (Typeable)

instance Widget (Frame a) where
  widget = widget.align
instance Widget (Link a) where
  visible = castAttr (align) visible
  refresh = refresh . align
