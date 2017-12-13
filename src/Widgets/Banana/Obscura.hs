{-# LANGUAGE RecursiveDo, RecordWildCards, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Widgets.Banana.Obscura where

import Widgets.Banana.Core
import Widgets.Banana.Links

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
