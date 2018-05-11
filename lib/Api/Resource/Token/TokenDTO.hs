module Api.Resource.Token.TokenDTO where

import Control.Lens (makeLenses)
import Data.Aeson

data TokenDTO = TokenDTO
  { _tdtoToken :: String
  }

makeLenses ''TokenDTO

instance ToJSON TokenDTO where
  toJSON TokenDTO {..} = object ["token" .= _tdtoToken]
