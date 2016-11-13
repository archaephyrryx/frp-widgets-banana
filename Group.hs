{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
module Widgets.Group where

import Widgets.Core hiding (Table, Row)
import Control.Monad (forM_, sequence_, forM, void)
import Graphics.UI.WX.Attributes (Attr)
import Widgets.Table (Item(..))
import Util ((<^>))
import Data.IORef

data Group = Group { _members :: [Item]
                   , _layout :: [Item] -> Layout
                   }
                   deriving (Typeable)

instance Widget Group where
  widget = _layout <^> _members

instance Visible Group where
  visible = newAttr "visible" getVis setVis
    where
      getVis :: Group -> IO Bool
      getVis r = or <$> forM (_members r) (\(Item x) -> get x visible)
      setVis :: Group -> Bool -> IO ()
      setVis r b = forM_ (_members r) (\(Item x) -> set x [ visible := b ])
  refresh r = do
    forM_ (_members r) (\(Item x) -> refresh x)

redrawGroup :: Group -> Event () -> MomentIO ()
redrawGroup g e = reactimate (refresh g <$ e)
