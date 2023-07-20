module Shared.Component.Api.Resource.Component.ComponentJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.Component.Model.Component.Component

instance ToJSON Component where
  toJSON = genericToJSON jsonOptions
