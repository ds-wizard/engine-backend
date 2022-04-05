module Wizard.Api.Resource.Plan.AppPlanJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Plan.AppPlan

instance FromJSON AppPlan where
  parseJSON = simpleParseJSON "_appPlan"

instance ToJSON AppPlan where
  toJSON = simpleToJSON "_appPlan"
