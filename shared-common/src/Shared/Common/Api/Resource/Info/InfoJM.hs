module Shared.Common.Api.Resource.Info.InfoJM where

import Data.Aeson

import Shared.Common.Api.Resource.Info.InfoDTO
import Shared.Common.Util.Aeson
import Shared.Component.Api.Resource.Component.ComponentJM ()

instance ToJSON InfoDTO where
  toJSON = genericToJSON jsonOptions

instance ToJSON InfoMetamodelVersionDTO where
  toJSON = genericToJSON jsonOptions
