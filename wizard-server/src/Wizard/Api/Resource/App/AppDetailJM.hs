module Wizard.Api.Resource.App.AppDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.App.AppDetailDTO
import Wizard.Api.Resource.Plan.AppPlanJM ()
import Wizard.Api.Resource.Usage.UsageJM ()
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON AppDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppDetailDTO where
  toJSON = genericToJSON jsonOptions
