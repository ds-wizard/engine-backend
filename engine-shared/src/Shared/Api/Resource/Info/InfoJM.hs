module Shared.Api.Resource.Info.InfoJM where

import Data.Aeson

import Shared.Api.Resource.Component.ComponentJM ()
import Shared.Api.Resource.Info.InfoDTO
import Shared.Util.Aeson

instance ToJSON InfoDTO where
  toJSON = genericToJSON jsonOptions
