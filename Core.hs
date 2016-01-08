{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, GADTs #-}

module Widgets.Core ( Prop''((:=~))
                    , module Widgets.Core
                    , module Widgets.Core.UI
                    , module Widgets.Core.FRP
                    ) where

import Widgets.Core.UI
import Widgets.Core.FRP
import Util (condense)

infixr 0 :=~

-- Wrapper for bundling UI elements and their reactive components
class Courier c t | c -> t where
  type Element c :: *
  tide :: c -> Tidings t
  element :: c -> Element c

data Prop'' w where
  (:=~) :: (Attr w a) -> StaticDynamic a -> Prop'' w

proper :: Prop'' w -> Bool
proper (a :=~ v) = isStatic v

toProp :: Prop'' w -> Prop w
toProp (a :=~ (Static v)) = (a := v)
toProp (a :=~ (Dynamic _)) = error "unable to convert Dynamic Prop'' to Prop"

toProp' :: Prop'' w -> Prop' w
toProp' (a :=~ (Dynamic v)) = (a :== v)
toProp' (a :=~ (Static _)) = error "unable to convert Static Prop'' to Prop' (cast to Dynamic, or convert to Prop)"

drown :: w -> [Prop'' w] -> MomentIO ()
drown w p'' =
  let
    (p, p') = condense proper toProp toProp' p''
  in do
    liftIO $ set w p
    sink w p'
