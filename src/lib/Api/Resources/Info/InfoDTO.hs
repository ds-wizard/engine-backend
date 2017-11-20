module Api.Resources.Info.InfoDTO where

import Control.Lens (makeLenses, (^.))
import Data.Aeson

data InfoDTO = InfoDTO
  { _idtoName :: String
  , _idtoVersion :: String
  , _idtoBuiltAt :: String
  }

makeLenses ''InfoDTO

instance ToJSON InfoDTO where
  toJSON InfoDTO {..} = object ["name" .= _idtoName, "version" .= _idtoVersion, "builtAt" .= _idtoBuiltAt]
