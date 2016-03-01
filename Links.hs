{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE PolyKinds             #-}
module Widgets.Links where

import Widgets.Core
import Util

-- | Unified SoftLink and LiquidLink
--
--  Because there is a natural division of UI elements and FRP logic in
--  reactive-banana, SoftLinks and LiquidLinks differ only in their
--  construction, not in their underlying structure. Therefore they are
--  consolidated into a single `Link` type, with the FRP logic dictated
--  by which constructor is used
data Link a = SoftLink { _link    :: LButton
                       , _crux    :: a
                       , _crucial :: Event a
                       }
            | LiquidLink { _link  :: LButton
                         , _flux  :: Behavior a
                         , _fluid :: Event a
                         }
                         deriving (Typeable)

data Coil a = Coil { _coil :: LButton
                   , _core :: StaticDynamic a
                   , _omen :: Event a
                   }

instance Widget (Link a) where
  widget = widget . _link

uncoil :: Coil a -> Link a
uncoil (Coil l c o) = corelink l o
    where
        corelink = cond isStatic (flip SoftLink . constant) (flip LiquidLink . variant) c

recoil :: Link a -> Coil a
recoil = Coil <$> _link <*> rubicon <*> blood

-- | crossing the stream
rubicon :: Link a -> StaticDynamic a
rubicon SoftLink{..} = Static _crux
rubicon LiquidLink{..} = Dynamic _flux

-- | crucial fluid
blood :: Link a -> Event a
blood (SoftLink{..}) = _crucial
blood (LiquidLink{..}) = _fluid

type LButton = Button ()

instance Courier (Link a) a where
    type Element (Link a) = LButton
    element = _link
    tide = (tidings . flux . rubicon) <*> blood

preLink :: Window w -> IO LButton
preLink w = button w []

softLink :: LButton
         -> (a -> String)
         -> a
         -> MomentIO (Link a)
softLink soft dval grist = do
  sink soft [ text :== (pure $ dval grist) ]
  click <- eClick soft

  let _coil = soft
      _core = Static grist
      _omen = grist <$ click
  return . uncoil $ Coil{..}

liquidLink :: LButton
           -> Behavior (a -> String)
           -> Behavior a
           -> MomentIO (Link a)
liquidLink liquid bdval fluid = do
    sink liquid [ text :== bdval <*> fluid ]
    click <- eClick liquid
    let _coil = liquid
        _core = Dynamic fluid
        _omen = fluid <@ click
    return . uncoil $ Coil{..}

linksTo, sinksTo :: Link a -> (a -> IO ()) -> MomentIO ()
sl`linksTo`f = case sl of
                 SoftLink{..} -> liftIO $ set _link [ on command := f _crux ]
ll`sinksTo`f = case ll of
                 LiquidLink{..} -> sink _link [ on command :== f <$> _flux ]
