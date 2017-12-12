module Reactive.StaticDynamic where

import Reactive.Banana
import Control.Applicative
import Util ((?.), (<^>))

-- | Data type representing either a pure value of a certain type or a Behavior on that type
--   Semantically,
--  >> StaticDynamic a == Either a (Behavior a)
data StaticDynamic a = Static  { constant ::          a }
                     | Dynamic { variant  :: Behavior a }

-- | Tests for constructor reflection
isStatic, isDynamic :: StaticDynamic a -> Bool
isStatic  (Static  _) = True ; isStatic  _ = False
isDynamic (Dynamic _) = True ; isDynamic _ = False

instance Functor StaticDynamic where
  f`fmap`(Static x) = Static (f x)
  f`fmap`(Dynamic bx) = Dynamic (f <$> bx)

-- | Leave a Dynamic value unchanged, casting a Static to a Dynamic on the pure behavior of its value
dyn :: StaticDynamic a -> StaticDynamic a
dyn = isStatic?.dynimate
  where
    dynimate = Dynamic . pure . constant

-- | Return a Behavior from a StaticDynamic, corresponding to a pure version of a Static value
flux :: StaticDynamic a -> Behavior a
flux = variant.dyn

-- | Return the pure value from a Static, undefined for dynamics
freeze :: StaticDynamic a -> a
freeze x = case x of
             Static a -> a
             Dynamic _ -> undefined


instance Applicative StaticDynamic where
  pure = Static
  sd <*> (Static x) = ($x) <$> sd
  (Static f) <*> sd = f <$> sd
  (Dynamic f) <*> (Dynamic x) = Dynamic (f <*> x)

combine :: StaticDynamic (a -> b) -> Behavior a -> Behavior b
combine (Static f) = (f<$>)
combine (Dynamic f) = (f<*>)

compose :: Behavior (a -> b) -> StaticDynamic a -> Behavior b
compose bf (Static x) = bf <^> x
compose bf (Dynamic x) = bf <*> x
