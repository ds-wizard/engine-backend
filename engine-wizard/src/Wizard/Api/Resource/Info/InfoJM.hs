module Wizard.Api.Resource.Info.InfoJM where

import Data.Aeson
import Wizard.Util.JSON (simpleToJSON)

import Wizard.Api.Resource.Info.InfoDTO

instance ToJSON InfoDTO where
  toJSON = simpleToJSON "_infoDTO"
