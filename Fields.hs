{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Widgets.Fields where

import Widgets.Core
import Widgets.Input
import Widgets.Table
import Widgets.Text
import Util hiding (Visible, visible)
import Control.Monad




data Field = Field { _labelString :: String
                   , _label :: RText
                   , _field :: TextInput
                   }
                   deriving (Typeable)

instance Widget Field where
  widget x = row 5 [ widget . _label $ x , widget . _field $ x ]

instance Courier Field String where
  type Element Field = RTextCtrl
  element = element . _field
  tide = tide . _field

field :: String -> RStaticText -> TextInput -> MomentIO Field
field str lab inp = do
  lab' <- frozenText lab str
  let _labelString = str
      _label = lab'
      _field = inp
   in return Field{..}

field' :: Window w -> String -> Behavior String -> MomentIO Field
field' w str bVal = do
  lab <- liftIO $ preText w
  inp <- input' w bVal
  field str lab inp


fieldML :: Window w -> String -> Behavior String -> MomentIO Field
fieldML w str bVal = do
  lab <- liftIO $ preText w
  inp <- inputML w bVal
  field str lab inp
