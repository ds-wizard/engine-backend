module Api.Resource.Event.EventFieldDTO where

import Control.Monad
import Data.Aeson

data EventFieldDTO a
  = NothingChangedDTO
  | ChangedValueDTO a
  deriving (Show, Eq)

instance Functor EventFieldDTO where
  fmap f (ChangedValueDTO a) = ChangedValueDTO (f a)
  fmap _ NothingChangedDTO = NothingChangedDTO

instance FromJSON a => FromJSON (EventFieldDTO a) where
  parseJSON (Object o) = do
    efChanged <- o .: "changed"
    if efChanged
      then do
        efValue <- o .: "value"
        return $ ChangedValueDTO efValue
      else return NothingChangedDTO
  parseJSON _ = mzero

instance ToJSON a => ToJSON (EventFieldDTO a) where
  toJSON (ChangedValueDTO efValue) = object ["changed" .= True, "value" .= efValue]
  toJSON NothingChangedDTO = object ["changed" .= False]
