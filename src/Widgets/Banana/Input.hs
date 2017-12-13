{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Widgets.Banana.Input where

import Widgets.Banana.Core
import Util hiding (Visible, visible)
import Control.Monad


type RTextCtrl = TextCtrl ()

data ValueInput a = ValueInput { _input :: RTextCtrl -- ^ UI component
                               , _value :: Tidings a
                               } deriving (Typeable)


type TextInput = ValueInput String

instance Widget (ValueInput a) where
  widget = widget . _input

instance Show (ValueInput a) where
  show = show . _input

instance Visible (ValueInput a) where
  visible = castAttr _input visible
  refresh = refresh . _input

instance Courier (ValueInput a) a where
  type Element (ValueInput a) = RTextCtrl
  element = _input
  tide = _value

preInput :: Window w -> IO RTextCtrl
preInput w = textEntry w []

preInputML :: Window w -> IO RTextCtrl
preInputML w = textCtrl w []

genInput :: RTextCtrl
         -> Behavior a
         -> (a -> String)
         -> (String -> Maybe a)
         -> MomentIO (ValueInput a)
genInput t bVal f g = do
  sink t [ text :== f <$> bVal ]
  eText <- eventText t

  let _input = t
      _value = tidings bVal $ filterJust $ g <$> eText
  return ValueInput{..}

input :: RTextCtrl
      -> Behavior String
      -> MomentIO TextInput
input t bVal = do
  sink t [ text :== bVal ]

  eText <- eventText t

  let _input = t
      _value = tidings bVal eText
  return ValueInput{..}

input' :: Window w -> Behavior String -> MomentIO TextInput
input' w bVal = do
  t <- liftIO $ preInput w
  input t bVal

inputML :: Window w -> Behavior String -> MomentIO TextInput
inputML w bVal = do
  t <- liftIO $ preInputML w
  input t bVal

intInput :: Window w -> Behavior Int -> MomentIO (ValueInput Int)
intInput w bVal = do
  t <- liftIO $ preInput w
  genInput t bVal show tryRead
