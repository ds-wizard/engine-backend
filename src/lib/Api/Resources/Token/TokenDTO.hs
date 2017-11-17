module Api.Resources.Token.TokenDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

data TokenDTO = TokenDTO
  { _tdtoToken :: String
  }

makeLenses ''TokenDTO

instance ToJSON TokenDTO where
  toJSON TokenDTO {..} = object ["token" .= _tdtoToken]
