{-# LANGUAGE AllowAmbiguousTypes       #-}
--{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
--{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE RankNTypes                #-}

module Widgets.Table where

import Widgets.Core hiding (Table, Row)
import Control.Monad (forM_, sequence_)

type Table = Panel ()

table :: Window w -> [Prop (Panel ())] -> IO Table
table w p = panel w p

data Tabular = Tabular { _table :: Table
                       , _rows :: Behavior [Row]
                       }


data Row = Row { _items :: [Item] }

data Item where
  Item :: (Typeable w, Widget w, Visible w) => w -> Item -- = forall w. (Typeable w, Widget w) => Item w

instance Widget Item where
  widget (Item x) = widget x
instance Visible Item where
  visible = revisit visible
    where
      revisit :: forall w. Attr w Bool -> Attr Item Bool
      revisit (Attr name mbdyn getter setter upd)
        = Attr name mbdyn (\(Item v) -> getter v)
                            (\(Item v) x -> setter v x)
                            (\(Item v) f -> upd v f)

  refresh (Item x) = refresh x

instance Widget Row where
    widget = row 5 . map widget . _items

visRow :: Behavior Bool -> Row -> MomentIO ()
visRow b r = forM_ (_items r) (\x -> sink x [ visible :== b ])

visRows :: [Behavior Bool] -> [Row] -> MomentIO ()
visRows bs rs = sequence_ $ zipWith visRow bs rs

tabulate :: Table
         -> Behavior [Row]
         -> MomentIO Tabular
tabulate tab bRows = do
    sink tab [ layout :== (column 5 . map widget) <$> bRows ]
    return $ Tabular tab bRows
