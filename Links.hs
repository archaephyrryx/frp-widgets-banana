{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Widgets.Links where

import Widgets.Core

-- | Unified SoftLink and LiquidLink
--
--  Because there is a natural division of UI elements and FRP logic in
--  reactive-banana, SoftLinks and LiquidLinks differ only in their
--  construction, not in their underlying structure. Therefore they are
--  consolidated into a single `Link` type, with the FRP logic dictated
--  by which constructor is used
data Link a = Link
  { link     :: LButton
  , actuated :: Tidings a
  }

type LButton = Button ()

instance Courier (Link a) a where
    type Element (Link a) = LButton
    element = link
    tide = actuated

softLink :: LButton
         -> (a -> String)
         -> a
         -> MomentIO (Link a)
softLink soft dval grist = do
  sink soft [ text :== (pure $ dval grist) ]
  click <- eClick soft
  let b = (pure grist)

  let actuated = tidings b $ b <@ click
      link = soft
  return $ Link{..}

liquidLink :: LButton
           -> Behavior (a -> String)
           -> Behavior a
           -> MomentIO (Link a)
liquidLink liquid bdval fluid = do
    sink liquid [ text :== bdval <*> fluid ]
    click <- eClick liquid
    let b = fluid
        actuated = tidings b $ b <@ click
        link = liquid
    return $ Link{..}
