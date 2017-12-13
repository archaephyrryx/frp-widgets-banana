{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
module Widgets.Banana.Radio where

import Widgets.Banana.Links
import Widgets.Banana.Table
import Widgets.Banana.Core
import Control.Monad (forM_, forM)



data Radio a = Radio { _radio :: Table
                     , _freq  :: [Link a]
                     , _tune :: Tidings a
                     }
                     deriving (Typeable)

instance Widget (Radio a) where
  widget = widget . _radio

instance Courier (Radio a) a where
  type Element (Radio a) = Table
  element = _radio
  tide = _tune

radio :: Eq a => Table -> [a] -> Behavior a -> (a -> String) -> MomentIO (Radio a)
radio w xs bx sf = do
    buts <- forM xs (\a -> liftIO (preLink (_tab w)) >>= \x -> softLink x sf a)
    liftIO $ set w [ layout := row 5 $ map widget buts ]

    let eDial = priorityUnion (map blood buts)

    forM_ buts (\x -> sink x [ enabled :== not <$> match x bx ])
    let _radio = w
        _freq = buts
        _tune = tidings bx eDial
    return $ Radio{..}
