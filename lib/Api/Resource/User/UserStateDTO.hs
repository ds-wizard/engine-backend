module Api.Resource.User.UserStateDTO where

import Control.Monad
import Data.Aeson

data UserStateDTO = UserStateDTO
  { _userStateDTOActive :: Bool
  }

instance FromJSON UserStateDTO where
  parseJSON (Object o) = do
    _userStateDTOActive <- o .: "active"
    return UserStateDTO {..}
  parseJSON _ = mzero

instance ToJSON UserStateDTO where
  toJSON UserStateDTO {..} = object ["active" .= _userStateDTOActive]
