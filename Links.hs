{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Widgets.Links where


import Widgets.Core
import Tidings

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Util

type SoftLink a = Button a

softLink :: forall a b t. Frameworks t
         => SoftLink b
         -> a      -- Value to hold
         -> Moment t (Tidings t a)
softLink soft grist = do
  click <- event0 soft command
  let b = (pure grist)
  return $ tidings b $ b <@ click

-- | Mutable-content softlink
type LiquidLink a = Button a

-- | The standard constructor for liquidlinks
liquidLink :: forall a b t. Frameworks t
           => LiquidLink b
           -> Behavior t (a -> String) -- Value to display
           -> Behavior t a -- Value to hold
           -> Moment t (Tidings t a)
liquidLink liquid bdval fluid = do
    sink liquid [ text :== bdval <*> fluid ]
    click <- event0 liquid command
    let b = fluid
    return $ tidings b $ b <@ click
