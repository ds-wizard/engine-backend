module Api.Resources.Version.VersionDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

data VersionDTO = VersionDTO
  { _vdtoDescription :: String
  }

makeLenses ''VersionDTO

instance FromJSON VersionDTO where
  parseJSON (Object o) = do
    _vdtoDescription <- o .: "description"
    return VersionDTO {..}
  parseJSON _ = mzero

instance ToJSON VersionDTO where
  toJSON VersionDTO {..} = object ["description" .= _vdtoDescription]
