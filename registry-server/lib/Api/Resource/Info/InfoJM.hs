module Api.Resource.Info.InfoJM where

import Data.Aeson

import Api.Resource.Info.InfoDTO
import Util.JSON (simpleToJSON)

instance ToJSON InfoDTO where
  toJSON = simpleToJSON "_infoDTO"
