module Api.Resource.User.UserPasswordDTO where

import Control.Lens (makeLenses)
import Control.Monad
import Data.Aeson

data UserPasswordDTO = UserPasswordDTO
  { _updtoPassword :: String
  }

makeLenses ''UserPasswordDTO

instance FromJSON UserPasswordDTO where
  parseJSON (Object o) = do
    _updtoPassword <- o .: "password"
    return UserPasswordDTO {..}
  parseJSON _ = mzero

instance ToJSON UserPasswordDTO where
  toJSON UserPasswordDTO {..} = object ["password" .= _updtoPassword]
