module Wizard.Api.Resource.Plan.AppPlanJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Plan.AppPlan

instance FromJSON AppPlan where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppPlan where
  toJSON = genericToJSON jsonOptions
