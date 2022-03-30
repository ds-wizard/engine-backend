module Wizard.Api.Resource.Plan.AppPlanChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Plan.AppPlanChangeDTO

instance FromJSON AppPlanChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AppPlanChangeDTO where
  toJSON = genericToJSON simpleOptions
