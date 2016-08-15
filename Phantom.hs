--{-# LANGUAGE AllowAmbiguousTypes       #-}
--{-# LANGUAGE FlexibleContexts          #-}
--{-# LANGUAGE FlexibleInstances         #-}
--{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
--{-# LANGUAGE NoMonoLocalBinds          #-}
--{-# LANGUAGE ImpredicativeTypes        #-}
--{-# LANGUAGE RankNTypes                #-}
module Widgets.Phantom where

import Widgets.Core hiding (Table, Row)
import Control.Monad (forM_, sequence_, forM, void)
import Graphics.UI.WX.Attributes (Attr)
import Widgets.Table (Item(..))
import Util ((<^>))
import Data.Array
import Data.Function (on)

data Aspect a = Edifice  { _construct :: Item
                         , _epithet   :: a
                         }
              | Artifice { _construct :: Item
                         , _epithet   :: a
                         , _transient :: Behavior Bool
                         }

aspect :: (Widget w, Visible w, Typeable w, Enum a) => w -> a -> Maybe (Behavior Bool) -> Aspect a
aspect c e (Just bv) = Artifice (Item c) e bv
aspect c e Nothing   = Edifice (Item c) e

eclipse :: (a -> Behavior Bool) -> Aspect a -> MomentIO ()
eclipse f (Edifice c e) = sink c [ visible :== (f e) ]
eclipse f (Artifice c e t) = sink c [ visible :== (&&) <$> f e <*> t ]


data Phantom a = Phantom { _aspects :: [Aspect a]
                         , _manifest :: Behavior a
                         }

instance Widget (Phantom a) where
  widget = row 5 . map (widget._construct) . _aspects

phantom :: (Enum a, Eq a) => [Aspect a] -> Behavior a -> MomentIO (Phantom a)
phantom as mf = do
  let nascence = (<$> mf) . (==)
  forM as (eclipse nascence)
  return $ Phantom as mf
