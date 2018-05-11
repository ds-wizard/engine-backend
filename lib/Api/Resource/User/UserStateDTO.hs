module Api.Resource.User.UserStateDTO where

import Control.Lens (makeLenses)
import Control.Monad
import Data.Aeson

data UserStateDTO = UserStateDTO
  { _usdtoActive :: Bool
  }

makeLenses ''UserStateDTO

instance FromJSON UserStateDTO where
  parseJSON (Object o) = do
    _usdtoActive <- o .: "active"
    return UserStateDTO {..}
  parseJSON _ = mzero

instance ToJSON UserStateDTO where
  toJSON UserStateDTO {..} = object ["active" .= _usdtoActive]
