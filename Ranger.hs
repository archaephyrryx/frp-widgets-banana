{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Widgets.Ranger where

import Util (andM)
import Widgets.Core

-- | A 'Ranger', which consists of an incrementor and decrementor for a
-- dynamic range and dynamic value within that range; works for any
-- bounded enumerable value
data Ranger a = Ranger
  { _rangeRG :: Range
  , _currentRG :: Tidings a
  }

data Range = Range
  { _prev :: Button ()
  , _cur  :: StaticText ()
  , _next :: Button ()
  } deriving (Typeable)

range' :: Window a -> [Prop (Button ())] -> [Prop (StaticText ())] -> [Prop (Button ())] -> IO (Range)
range' f p c n = Range <$> button f p <*> staticText f c <*> button f n

range :: Window a -> IO (Range)
range f = range' f [ text := "<" ] [] [ text := ">" ]

instance Widget Range where
    widget w = margin 10 $ row 5 $ map ($w) [widget._prev, widget._cur, widget._next]

instance Courier (Ranger a) a where
  type Element (Ranger a) = Range
  tide = _currentRG
  element = _rangeRG

instance Visible Range where
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



-- | Create a 'RelNav'.
ranger :: (Ord a, Enum a)
    => Range
    -> Behavior a -- ^ Current location
    -> Behavior a -- ^ 'Zero' value
    -> Behavior a -- ^ Maximum value (zero-indexed)
    -> Behavior (a -> RenderedValue) -- ^ display for an item
    -> MomentIO (Ranger a)
ranger range bloc bzer bmax bdisplay = do
    let (Range prev cur next) = range
    let bNotFirst = (>) <$> bloc <*> bzer
        bNotLast  = (<) <$> bloc <*> bmax

    sink prev [ enabled :== bNotFirst ]
    sink cur [ curview :== bdisplay <*> bloc ]
    sink next [ enabled :== bNotLast ]

    ePrev <- eClick prev
    eNext <- eClick next

    let
        eDelta :: forall n. Enum n => Event (n -> n)
        eDelta = unions [ pred <$ whenE bNotFirst ePrev
                        , succ <$ whenE bNotLast  eNext ]
        eChange = flip ($) <$> bloc <@> eDelta

        _currentRG = tidings bloc eChange
        _rangeRG   = range
    return Ranger{..}

curview :: Textual w => Attr w RenderedValue
curview = writeAttr "curview" $ \x i -> do
  let j = renderValue i
  set x [ text := j ]
  return ()
