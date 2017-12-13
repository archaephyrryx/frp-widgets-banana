{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GADTs                 #-}
module Widgets.Banana.Frame where

import Widgets.Banana.Core hiding (Row)
import Widgets.Banana.Table
import Widgets.Banana.Links

data GenItem a where
  GenItem :: (Typeable (w a), Widget (w a), Visible (w a), Courier (w a) a) => w a -> GenItem a
  Internal :: (Typeable w, Widget w, Visible w) => w -> GenItem a

convert :: GenItem a -> Item
convert (GenItem x) = Item x
convert (Internal x) = Item x

align :: SyncFrame a -> Row
align = Row . map convert . _tiles

data SyncFrame a = SyncFrame { _tiles :: [GenItem a]
                             , _subject :: Tidings a
                             }
                             deriving (Typeable)

instance Widget (SyncFrame a) where
  widget = widget.align
instance Visible (SyncFrame a) where
  visible = castAttr align visible
  refresh = refresh . align
