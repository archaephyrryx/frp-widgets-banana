--{-# LANGUAGE AllowAmbiguousTypes       #-}
--{-# LANGUAGE FlexibleContexts          #-}
--{-# LANGUAGE FlexibleInstances         #-}
--{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
--{-# LANGUAGE NoMonoLocalBinds          #-}
--{-# LANGUAGE ImpredicativeTypes        #-}
--{-# LANGUAGE RankNTypes                #-}
module Widgets.Table where

import Widgets.Core hiding (Table, Row)
import Control.Monad (forM_, sequence_, forM, void)
import Graphics.UI.WX.Attributes (Attr)

type Table = Panel ()

table :: Window w -> [Prop (Panel ())] -> IO Table
table w p = panel w p

data Tabular = Tabular { _table :: Table
                       , _rows :: Behavior [Row]
                       }

data Row = Row { _items :: [Item] }
  deriving (Typeable)

data Item where
  Item :: (Typeable w, Widget w, Visible w) => w -> Item -- = forall w. (Typeable w, Widget w) => Item w

instance Widget Item where
  widget (Item x) = widget x

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
    sink tab [ layout :== (column 5 . map widget) <$> bRows ]
    return $ Tabular tab bRows
