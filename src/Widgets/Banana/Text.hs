{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Widgets.Banana.Text where

import Widgets.Banana.Core
import Util hiding (Visible, visible)
import Reactive.ValText
import Control.Monad

type RStaticText = StaticText ()

data RText = RText { _box :: RStaticText -- ^ UI component
                   , _msg :: StaticDynamic String     -- ^ Dynamic contents
                   , _dlt :: Event ()    -- ^ Update event
                   }
                   deriving (Typeable)

instance Widget RText where
  widget = widget . _box

instance Show RText where
  show = show . _box

instance Visible RText where
  visible = castAttr _box visible
  refresh = refresh . _box

instance Courier RText String where
  type Element RText = RStaticText
  element = _box
  tide = (.(flux . _msg)) =<< liftM2 tidings id . flip (<@) . _dlt

preText :: Window w -> IO RStaticText
preText w = staticText w []

frozenText :: RStaticText
           -> String
           -> MomentIO RText
frozenText t s = do
  liftIO $ set t [ text := s ]
  let _box = t
      _msg = Static s
      _dlt = never
  return RText{..}


frozenText' :: Window w -> String -> MomentIO RText
frozenText' w s = do
  t <- liftIO $ preText w
  frozenText t s

rText :: RStaticText
      -> StaticDynamic String
      -> Event ()
      -> MomentIO RText
rText t str sync = do
  drown t [ text :=~ str ]
  let _box = t
      _msg = str
      _dlt = sync
  return RText{..}

rText' :: RStaticText
       -> ValText
       -> Event ()
       -> MomentIO RText
rText' t vt sync =
  let str = sdText vt
   in rText t str sync
