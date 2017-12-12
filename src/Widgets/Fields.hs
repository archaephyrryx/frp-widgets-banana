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


data Field a = Field { _labelString :: String
                     , _label :: RText
                     , _field :: ValueInput a
                     } deriving (Typeable)

type TextField = Field String

instance Widget (Field a) where
  widget x = row 5 [ widget . _label $ x , widget . _field $ x ]

instance Courier (Field a) a where
  type Element (Field a) = RTextCtrl
  element = element . _field
  tide = tide . _field

instance Visible (Field a) where
  visible = newAttr "visible" getVis setVis
    where
      getVis :: Field a -> IO Bool
      getVis r = afor (get (_label r) visible) (get (_field r) visible)
      setVis :: Field a -> Bool -> IO ()
      setVis r b = set (_label r) [ visible := b ] >> set (_field r) [ visible := b ]
  refresh = (>>) <$> refresh._label <*> refresh._field



field :: String -> RStaticText -> ValueInput a -> MomentIO (Field a)
field str lab inp = do
  lab' <- frozenText lab str
  let _labelString = str
      _label = lab'
      _field = inp
   in return Field{..}

field' :: Window w -> String -> Behavior String -> MomentIO TextField
field' w str bVal = do
  lab <- liftIO $ preText w
  inp <- input' w bVal
  field str lab inp


fieldML :: Window w -> String -> Behavior String -> MomentIO TextField
fieldML w str bVal = do
  lab <- liftIO $ preText w
  inp <- inputML w bVal
  field str lab inp

intField :: Window w -> String -> Behavior Int -> MomentIO (Field Int)
intField w str bVal = do
  lab <- liftIO $ preText w
  inp <- intInput w bVal
  field str lab inp
