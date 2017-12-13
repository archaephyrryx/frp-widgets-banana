{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Widgets.Banana.Recorder where

import Widgets.Banana.Core
import Util hiding (Visible, visible)
import Control.Monad
import Widgets.Banana.Input
import Widgets.Banana.Text
import Widgets.Banana.Table
import Widgets.Banana.Fields
import Widgets.Banana.Links

data Recorder = Recorder { _save :: Link String
                         , _load :: Link ()
                         , _store :: Tidings String
                         }
                         deriving (Typeable)


instance Widget Recorder where
  widget Recorder{..} = row 5 [ widget _save , widget _load ]

instance Courier Recorder String where
  type Element Recorder = (Link String, Link ())
  element = (,) <$> _save <*> _load
  tide = _store

recorder :: Window w -- ^ Window to put buttons in
         -> Behavior String -- ^ String value to be saved/loaded
         -> Behavior String -- ^ String value to display for save button
         -> String -- ^ String value to display for load button
         -> Behavior String -- ^ Filepath to save to / load from
         -> MomentIO Recorder
recorder w bVal bLabs labl bPath = do
  _save <- liftIO (preLink w) >>= \l -> liquidLink l (const <$> bLabs) bVal
  _load <- liftIO (preLink w) >>= (`voidLink`labl)
  sink (_link _save) [ on command :== writeFile <$> bPath <*> bVal ]
  eLoad <- mapEventIO readFile (bPath <@ portents _load)
  let _store = tidings bVal eLoad
  return Recorder{..}
