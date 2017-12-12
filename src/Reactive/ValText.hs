{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE RecordWildCards           #-}
module Reactive.ValText where

import Reactive.StaticDynamic
import Util (unity)
import Control.Applicative
import Reactive.Banana

data ValText = forall a.
     ValText { _val :: StaticDynamic a
             , _fun :: StaticDynamic (a -> String)
             }

sdText :: ValText -> StaticDynamic String
sdText (ValText{..}) = _fun <*> _val

fixed :: forall a. a -> (a -> String) -> ValText
fixed x f = ValText { _val = Static x, _fun = Static f }

mask :: forall a. Behavior a -> (a -> String) -> ValText
mask b f = ValText { _val = Dynamic b, _fun = Static f }

lens :: forall a. a -> Behavior (a -> String) -> ValText
lens x b = ValText { _val = Static x, _fun = Dynamic b }

volatile :: forall a. Behavior a -> Behavior (a -> String) -> ValText
volatile x f = ValText { _val = Dynamic x, _fun = Dynamic f }

ignorant :: StaticDynamic String -> ValText
ignorant sd = ValText { _val = Static (), _fun = unity`fmap`sd }
