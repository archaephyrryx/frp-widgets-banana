{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GADTs                 #-}
module Widgets.Frame where

import Widgets.Core hiding (Row)
import Widgets.Table
import Widgets.Links

type (Internal m) a = m

data GenItem a where
  GenItem :: (Typeable (w a), Widget (w a), Visible (w a)) => w a -> GenItem a

align :: SyncFrame a -> Row
align = Row . map (\(GenItem x) -> (Item x)) . _tiles

data SyncFrame a = SyncFrame { _tiles :: [GenItem a]
                             , _subject :: Behavior a
                             }
                             deriving (Typeable)

instance Widget (SyncFrame a) where
  widget = widget.align
instance Visible (SyncFrame a) where
  visible = castAttr (align) visible
  refresh = refresh . align
