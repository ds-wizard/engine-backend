module Api.Resource.Info.InfoDTO where

import Data.Aeson

data InfoDTO = InfoDTO
  { _idtoName :: String
  , _idtoVersion :: String
  , _idtoBuiltAt :: String
  }

instance ToJSON InfoDTO where
  toJSON InfoDTO {..} = object ["name" .= _idtoName, "version" .= _idtoVersion, "builtAt" .= _idtoBuiltAt]
