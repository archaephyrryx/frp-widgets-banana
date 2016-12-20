{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE TypeFamilies          #-}

module Widgets.Counter  where

import Util (andM)
import Widgets.Core

-- | A counter widget based loosely on the logic of 'Ranger', without any display for the value
-- (pure control). Variants allow for just an incrementor, just a decrementor, a +/- counter, and
-- resettable variants.
--   Incrementor has no decrement
--   Decrementor has no increment
--   Counter has both

data Counter a = Counter
  { _ctrlCTR :: FullControl
  , _valCTR :: Tidings a
  }
  deriving (Typeable)

instance Widget (Counter a) where
  widget = widget . _ctrlCTR
instance Visible (Counter a) where
  visible = castAttr _ctrlCTR visible
  refresh = refresh . _ctrlCTR

data FullControl
  = Incrementor { _inc :: Button () }
    deriving (Typeable)


incre' :: Window a -> [Prop (Button ())] -> IO (FullControl)
incre' f i = Incrementor <$> button f i

incre :: Window a -> IO (FullControl)
incre f = incre' f [ text := "+" ]

instance Widget FullControl where
    widget w = margin 10 $ row 5 $ map ($w) [widget._inc]

instance Courier (Counter a) a where
  type Element (Counter a) = FullControl
  tide = _valCTR
  element = _ctrlCTR

instance Visible FullControl where
  visible = castAttr _inc visible
  refresh = refresh . _inc
  {-
  visible = newAttr "visible" getVis setVis
    where
      getVis :: Range -> IO Bool
      getVis =
        let f :: Visible a => a -> IO Bool
            f = (flip get visible)
                in ((andM.).andM) <$> f._prev
                                  <*> f._cur
                                  <*> f._next
      setVis :: Range -> Bool -> IO ()
      setVis r b =
        let f :: Visible a => a -> IO ()
            f = flip set [ visible := b ]
         in (((>>).).(>>)) <$> f._prev
                           <*> f._cur
                           <*> f._next $ r

  refresh = (((>>).).(>>)) <$> refresh._prev
                           <*> refresh._cur
                           <*> refresh._next

-}

counter :: (Ord a, Enum a)
        => FullControl
        -> Behavior a
        -> StaticDynamic a
        -> MomentIO (Counter a)
counter fc val mx = counter' fc succ val mx


-- | Create a 'Counter'.
counter' :: (Ord a, Enum a)
        => FullControl
        -> (a -> a) -- ^ Increment
        -> Behavior a -- ^ Current value
        -> StaticDynamic a -- ^ Maximum value
        -> MomentIO (Counter a)
counter' control step bVal sdMax = do
  let (Incrementor inc) = control

  let bUnder  = ((<)<$>bVal) `compose` sdMax

  sink inc [ enabled :== bUnder ]

  eInc <- eClick inc

  let eDelta = unions [ step <$ whenE bUnder eInc ]
      eChange = flip ($) <$> bVal <@> eDelta

      _valCTR  = tidings bVal eChange
      _ctrlCTR = control
  return Counter{..}
