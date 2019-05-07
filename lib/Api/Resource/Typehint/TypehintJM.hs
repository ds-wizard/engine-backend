module Api.Resource.Typehint.TypehintJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Typehint.TypehintDTO

instance FromJSON TypehintDTO where
  parseJSON (Object o) = do
    _typehintDTOIntId <- o .: "id"
    _typehintDTOName <- o .: "name"
    _typehintDTOUrl <- o .: "url"
    return TypehintDTO {..}
  parseJSON _ = mzero

instance ToJSON TypehintDTO where
  toJSON TypehintDTO {..} = object ["id" .= _typehintDTOIntId, "name" .= _typehintDTOName, "url" .= _typehintDTOUrl]
