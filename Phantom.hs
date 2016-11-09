--{-# LANGUAGE AllowAmbiguousTypes       #-}
--{-# LANGUAGE FlexibleContexts          #-}
--{-# LANGUAGE FlexibleInstances         #-}
--{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
--{-# LANGUAGE NoMonoLocalBinds          #-}
--{-# LANGUAGE ImpredicativeTypes        #-}
--{-# LANGUAGE RankNTypes                #-}

-- | A module for displaying one of several different widgets in a static frame, modally
module Widgets.Phantom where

import Widgets.Core hiding (Table, Row)
import Control.Monad (forM_, sequence_, forM, void)
import Graphics.UI.WX.Attributes (Attr)
import Widgets.Table (Item(..))
import Util ((<^>))
import Data.Array
import Data.Function (on)

-- | A widget class for one possible contents of a static frame. `Edifice` defines contents that are
-- visible whenever they are in focus, while `Artifice` defines contents that may be invisible even
-- while in focus. Each aspect should contain a unique equatable polymorphic tag so that it may be
-- invoked by the `Phantom` widget.
data Aspect a =  Edifice { _construct :: Item
                         , _epithet   :: a
                         }
              | Artifice { _construct :: Item
                         , _epithet   :: a
                         , _transient :: Behavior Bool
                         }

-- | Unified constructor for the `Aspect` data-type, with a Maybe-enclosed boolean behavior, which
-- serves as the transience parameter for Artifices when present
aspect :: (Widget w, Visible w, Typeable w, Enum a) => w -> a -> Maybe (Behavior Bool) -> Aspect a
aspect c e (Just bv) = Artifice (Item c) e bv
aspect c e Nothing   = Edifice (Item c) e

-- | `eclipse` defines the conditional visibility of an `Aspect` in the MomentIO monad.
eclipse :: (a -> Behavior Bool) -> Aspect a -> MomentIO ()
eclipse f (Edifice c e) = sink c [ visible :== (f e) ]
eclipse f (Artifice c e t) = sink c [ visible :== (&&) <$> f e <*> t ]


-- | Widget consisting of all of the possible aspects of a static frame, which focuses only one at a
-- time (though the focused aspect may be invisible)
data Phantom a = Phantom { _aspects :: [Aspect a]
                         , _manifest :: Behavior a
                         }

instance Widget (Phantom a) where
  widget = row 5 . map (widget._construct) . _aspects

-- | Create a `Phantom` widget from a list of aspects and an invocation behavior
phantom :: (Enum a, Eq a) => [Aspect a] -> Behavior a -> MomentIO (Phantom a)
phantom as mf = do
  let nascence = (<$> mf) . (==)
  forM as (eclipse nascence)
  return $ Phantom as mf
