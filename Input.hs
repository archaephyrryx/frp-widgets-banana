{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Widgets.Input where

import Widgets.Core
import Util hiding (Visible, visible)
import Control.Monad

type RTextCtrl = TextCtrl ()

data TextInput = TextInput { _input :: RTextCtrl -- ^ UI component
                           , _value :: Tidings String
                           }
                           deriving (Typeable)

instance Widget TextInput where
  widget = widget . _input

instance Show TextInput where
  show = show . _input

instance Visible TextInput where
  visible = castAttr _input visible
  refresh = refresh . _input

instance Courier TextInput String where
  type Element TextInput = RTextCtrl
  element = _input
  tide = _value

preInput :: Window w -> IO RTextCtrl
preInput w = textEntry w []

preInputML :: Window w -> IO RTextCtrl
preInputML w = textCtrl w []

input :: RTextCtrl
      -> Behavior String
      -> MomentIO TextInput
input t bVal = do
  sink t [ text :== bVal ]

  eText <- eventText t

  let _input = t
      _value = tidings bVal eText
  return TextInput{..}

input' :: Window w -> Behavior String -> MomentIO TextInput
input' w bVal = do
  t <- liftIO $ preInput w
  input t bVal

inputML :: Window w -> Behavior String -> MomentIO TextInput
inputML w bVal = do
  t <- liftIO $ preInputML w
  input t bVal
