{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, GADTs #-}

module Widgets.Core ( Prop''((:=~))
                    , module Widgets.Core
                    , module Data.Typeable
                    , module Widgets.Core.UI
                    , module Widgets.Core.FRP
                    ) where

import Widgets.Core.UI
import Widgets.Core.FRP
import Data.Typeable
import Util (condense)

infixr 0 :=~

-- | Courier - a bearer of tidings
-- Courier types are wrappers for bundling UI elements with their reactive components
class Courier c t | c -> t where
  type Element c :: *
  element :: c -> Element c
  tide :: c -> Tidings t
  tide = tidings <$> omens <*> portents
  -- Optional declarations
  omens :: c -> Behavior t
  omens = facts . tide
  portents :: c -> Event t
  portents = rumors . tide

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

eClick :: Commanding w => w -> MomentIO (Event ())
eClick x = event0 x command

relay :: Window a -> Event () -> MomentIO ()
relay w e = reactimate $
  (windowReLayout w) <$ e
