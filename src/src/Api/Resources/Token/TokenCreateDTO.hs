module Api.Resources.Token.TokenCreateDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

data TokenCreateDTO = TokenCreateDTO
  { _tcdtoEmail :: String
  , _tcdtoPassword :: String
  }

makeLenses ''TokenCreateDTO

instance FromJSON TokenCreateDTO where
  parseJSON (Object o) = do
    _tcdtoEmail <- o .: "email"
    _tcdtoPassword <- o .: "password"
    return TokenCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON TokenCreateDTO where
  toJSON TokenCreateDTO {..} =
    object ["email" .= _tcdtoEmail, "password" .= _tcdtoPassword]
