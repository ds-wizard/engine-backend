module Shared.Api.Resource.Component.ComponentJM where

import Data.Aeson

import Shared.Model.Component.Component
import Shared.Util.Aeson

instance ToJSON Component where
  toJSON = genericToJSON jsonOptions
