module Wizard.Api.Resource.Plan.AppPlanJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Model.Plan.AppPlan

instance FromJSON AppPlan where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppPlan where
  toJSON = genericToJSON jsonOptions
