--{-# LANGUAGE AllowAmbiguousTypes       #-}
--{-# LANGUAGE FlexibleContexts          #-}
--{-# LANGUAGE FlexibleInstances         #-}
--{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
--{-# LANGUAGE NoMonoLocalBinds          #-}
--{-# LANGUAGE ImpredicativeTypes        #-}
--{-# LANGUAGE RankNTypes                #-}
module Widgets.Banana.Table where

import Widgets.Banana.Core hiding (Table, Row)
import Control.Monad (forM_, sequence_, forM, void)
import Graphics.UI.WX.Attributes (Attr)
import Util (mapend, reveal)

newtype Table = Table { _tab :: Panel () } deriving (Typeable)

instance Widget Table where
  widget = margin 10 . boxed "Table" . dynamic . widget . _tab

instance Form Table where
  layout = castAttr _tab layout

instance Visible Table where
  visible = castAttr _tab visible
  refresh = refresh . _tab


table :: Window w -> [Prop (Panel ())] -> IO Table
table w p = panel w p >>= return.Table

data Tabular = Tabular { _table :: Table
                       , _rows :: Behavior [Row]
                       }

data Row = Row { _items :: [Item] }
  deriving (Typeable, Show)

data Item where
  Item :: (Typeable w, Widget w, Visible w) => w -> Item -- = forall w. (Typeable w, Widget w) => Item w

instance Show Item where
  show (Item x) = reveal x

instance Widget Item where
  widget (Item x) = widget x

instance Visible Item where
  visible = newAttr "visible" getVis setVis
    where
      getVis (Item x) = get x visible
      setVis (Item x) b = set x [ visible := b ]
  refresh (Item x) = refresh x

instance Widget Row where
    widget = row 5 . map widget . _items

instance Visible Row where
  visible = newAttr "visible" getVis setVis
    where
      getVis :: Row -> IO Bool
      getVis r = or <$> forM (_items r) (\(Item x) -> get x visible)
      setVis :: Row -> Bool -> IO ()
      setVis r b = forM_ (_items r) (\(Item x) -> set x [ visible := b ])
  refresh r = do
    forM_ (_items r) (\(Item x) -> refresh x)

redrawRows :: Behavior ([Row] -> [Bool]) -> Behavior [Row] -> MomentIO ()
redrawRows bf br = do
  let
      bact :: Behavior (IO ())
      bact = (\f r -> let b = f r in sequence_ $ zipWith (\v x -> set x [ visible := v ] >> refresh x) b r) <$> bf <*> br
  reactimate' =<< changes bact

tabulate :: Table
         -> Behavior [Row]
         -> MomentIO Tabular
tabulate tab bRows = do
  sink tab [ layout :== column 5 . map widget <$> bRows ]
  return $ Tabular tab bRows
