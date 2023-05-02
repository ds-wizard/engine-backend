module Shared.Common.Api.Resource.Component.ComponentJM where

import Data.Aeson

import Shared.Common.Model.Component.Component
import Shared.Common.Util.Aeson

instance ToJSON Component where
  toJSON = genericToJSON jsonOptions
