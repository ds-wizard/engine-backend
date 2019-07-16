module Api.Resource.Info.InfoJM where

import Data.Aeson
import Util.JSON (simpleToJSON)

import Api.Resource.Info.InfoDTO

instance ToJSON InfoDTO where
  toJSON = simpleToJSON "_infoDTO"
