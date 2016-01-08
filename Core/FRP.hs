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
import Util ((?.))

data StaticDynamic a = Static { constant :: a }
                     | Dynamic { variant :: (Behavior a) }

isStatic, isDynamic :: StaticDynamic a -> Bool
isStatic  (Static  _) = True ; isStatic  _ = False
isDynamic (Dynamic _) = True ; isDynamic _ = False

instance Functor StaticDynamic where
  f`fmap`(Static x) = Static (f x)
  f`fmap`(Dynamic bx) = Dynamic (f <$> bx)

dyn :: StaticDynamic a -> StaticDynamic a
dyn = isStatic?.dynimate
  where
    dynimate = Dynamic . pure . constant

flux :: StaticDynamic a -> Behavior a
flux = variant.dyn

instance Applicative StaticDynamic where
  pure = Static
  sd <*> (Static x) = ($x) <$> sd
  (Static f) <*> sd = f <$> sd
  (Dynamic f) <*> (Dynamic x) = Dynamic (f <*> x)
