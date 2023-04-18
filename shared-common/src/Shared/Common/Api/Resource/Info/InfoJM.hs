module Shared.Common.Api.Resource.Info.InfoJM where

import Data.Aeson

import Shared.Common.Api.Resource.Component.ComponentJM ()
import Shared.Common.Api.Resource.Info.InfoDTO
import Shared.Common.Util.Aeson

instance ToJSON InfoDTO where
  toJSON = genericToJSON jsonOptions
