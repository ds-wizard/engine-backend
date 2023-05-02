module Wizard.Api.Resource.Plan.AppPlanChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Plan.AppPlanChangeDTO

instance FromJSON AppPlanChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppPlanChangeDTO where
  toJSON = genericToJSON jsonOptions
