{-# LANGUAGE RecursiveDo, RecordWildCards, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Widgets.Obscura where

import Widgets.Core
import Widgets.Links

type Obscure = BitmapButton ()

data Obscura a = Obscura
               { _image :: Obscure
               , _snapshots :: Tidings a
               }

instance Courier (Obscura a) a where
  type Element (Obscura a) = Obscure
  element = _image
  tide = _snapshots

obscura :: Obscure
        -> Behavior (a -> String)
        -> Behavior a
        -> MomentIO (Obscura a)
obscura obs bLink fluid = do
    sink obs [ picture :== (bLink <*> fluid) ]
    click <- eClick obs

    let b = fluid
        _snapshots = tidings b $ b <@ click
        _image = obs
    return Obscura{..}
