{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TypeFamilies #-}
module Widgets.Links where

import Widgets.Core

-- | Unified SoftLink and LiquidLink
--
--  Because there is a natural division of UI elements and FRP logic in
--  reactive-banana, SoftLinks and LiquidLinks differ only in their
--  construction, not in their underlying structure. Therefore they are
--  consolidated into a single `Link` type, with the FRP logic dictated
--  by which constructor is used
data Link t a = Link
  { link     :: LButton
  , actuated :: Tidings t a
  }

type LButton = Button ()

instance forall a t. Courier (Link t a) where
    type Tidal (Link t a) = a
    type Element (Link t a) = LButton
    type Temporal (Link t a) = t
    element = link
    tide = actuated

softLink :: forall a b t. Frameworks t
         => LButton
         -> (a -> String)
         -> a
         -> Moment t (Link t a)
softLink soft dval grist = do
  sink soft [ text :== (pure $ dval grist) ]
  click <- event0 soft command
  let b = (pure grist)

  let actuated = tidings b $ b <@ click
      link = soft
  return $ Link{..}

liquidLink :: forall a b t. Frameworks t
           => LButton
           -> Behavior t (a -> String)
           -> Behavior t a
           -> Moment t (Link t a)
liquidLink liquid bdval fluid = do
    sink liquid [ text :== bdval <*> fluid ]
    click <- event0 liquid command
    let b = fluid
        actuated = tidings b $ b <@ click
        link = liquid
    return $ Link{..}
