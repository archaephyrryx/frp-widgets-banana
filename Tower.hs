{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}

module Widgets.Tower where

import Widgets.Core hiding (Table, Row)
import Widgets.Table (Table)

data Tower = forall w. (Widget w) =>
     Tower { _pile :: Table
           , _heap :: Behavior [w]
           , _stack :: [w] -> Layout
           }
           deriving (Typeable)

instance Widget Tower where
  widget = widget . _pile
instance Visible Tower where
  visible = castAttr _pile visible
  refresh = refresh . _pile

erect :: Widget w => Table -> Behavior [w] -> ([w] -> Layout) -> MomentIO Tower
erect t b s = do
  sink t [ layout :== s <$> b ]
  return $ Tower t b s
