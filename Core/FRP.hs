module Widgets.Core.FRP ( module Widgets.Core.FRP
                        , module Reactive.Tidings
                        , module Reactive.Banana
                        , module Reactive.Banana.WX
                        , module Reactive.Banana.Frameworks
                        ) where

import Reactive.Tidings
import Reactive.Banana
import Reactive.Banana.WX
import Reactive.Banana.Frameworks
import Control.Applicative

data StaticDynamic a = Static a
                     | Dynamic (Behavior a)

instance Functor StaticDynamic where
  f`fmap`(Static x) = Static (f x)
  f`fmap`(Dynamic bx) = Dynamic (f <$> bx)

dyn :: StaticDynamic a -> StaticDynamic a
dyn (Static x) = Dynamic (pure x)
dyn x@(Dynamic _) = x

flux :: StaticDynamic a -> Behavior a
flux (Static x) = pure x
flux (Dynamic bx) = bx

instance Applicative StaticDynamic where
  pure = Static
  sd <*> (Static x) = ($x) <$> sd
  (Static f) <*> sd = f <$> sd
  (Dynamic f) <*> (Dynamic x) = Dynamic (f <*> x)
