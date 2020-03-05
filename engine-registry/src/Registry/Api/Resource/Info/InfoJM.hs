module Registry.Api.Resource.Info.InfoJM where

import Data.Aeson

import Registry.Api.Resource.Info.InfoDTO
import Shared.Util.JSON (simpleToJSON)

instance ToJSON InfoDTO where
  toJSON = simpleToJSON "_infoDTO"
