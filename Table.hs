{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}

module Widgets.Table where

import Widgets.Core hiding (Table, Row)

type Table = Panel ()

table :: Window w -> [Prop (Panel ())] -> IO Table
table w p = panel w p

data Tabular = Tabular { _table :: Table
                       , _rows :: Behavior [Row]
                       }


data Row = Row { _items :: [Item] }

data Item = forall w. (Typeable w, Widget w) => Item w

instance Widget Item where
    widget (Item x) = widget x

instance Widget Row where
    widget = row 5 . map widget . _items

tabulate :: Table
         -> Behavior [Row]
         -> MomentIO Tabular
tabulate tab bRows = do
    sink tab [ layout :== (column 5 . map widget) <$> bRows ]
    return $ Tabular tab bRows
