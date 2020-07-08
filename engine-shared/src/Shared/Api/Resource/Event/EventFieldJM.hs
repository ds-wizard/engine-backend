module Shared.Api.Resource.Event.EventFieldJM where

import Control.Monad
import Data.Aeson

import Shared.Model.Event.EventField

instance FromJSON a => FromJSON (EventField a) where
  parseJSON (Object o) = do
    efChanged <- o .: "changed"
    if efChanged
      then do
        efValue <- o .: "value"
        return $ ChangedValue efValue
      else return NothingChanged
  parseJSON _ = mzero

instance ToJSON a => ToJSON (EventField a) where
  toJSON (ChangedValue efValue) = object ["changed" .= True, "value" .= efValue]
  toJSON NothingChanged = object ["changed" .= False]
