module Api.Resource.ActionKey.ActionKeyDTO where

import Control.Monad
import Data.Aeson

data ActionKeyDTO = ActionKeyDTO
  { _actionKeyDTOAType :: String
  , _actionKeyDTOEmail :: String
  } deriving (Show, Eq)

instance FromJSON ActionKeyDTO where
  parseJSON (Object o) = do
    _actionKeyDTOAType <- o .: "type"
    _actionKeyDTOEmail <- o .: "email"
    return ActionKeyDTO {..}
  parseJSON _ = mzero

instance ToJSON ActionKeyDTO where
  toJSON ActionKeyDTO {..} = object ["type" .= _actionKeyDTOAType, "email" .= _actionKeyDTOEmail]
