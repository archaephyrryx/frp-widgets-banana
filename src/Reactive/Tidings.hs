module Reactive.Tidings
  ( Tidings
  , tidings
  , facts
  , rumors
  ) where

import Reactive.Banana.Combinators

data Tidings a
  = T { facts  :: Behavior a
      , rumors :: Event    a
      }

tidings :: Behavior a -> Event a -> Tidings a
tidings b e = T b e

instance Functor Tidings where
  fmap f (T b e) = T (fmap f b) (fmap f e)

instance Applicative Tidings where
  pure x = T (pure x) never
  f <*> x = uncurry ($) <$> pair f x

pair :: Tidings a -> Tidings b -> Tidings (a,b)
pair (T bx ex) (T by ey) = T b e
  where
    b = (,) <$> bx <*> by
    x = flip (,) <$> by <@> ex
    y = (,) <$> bx <@> ey
    e = unionWith (\(x,_) (_,y) -> (x,y)) x y
