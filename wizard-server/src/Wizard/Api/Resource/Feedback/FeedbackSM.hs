module Wizard.Api.Resource.Feedback.FeedbackSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Api.Resource.Feedback.FeedbackJM ()
import Wizard.Database.Migration.Development.Feedback.Data.Feedbacks
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Model.Config.ServerConfigDM hiding (defaultProject)
import Wizard.Service.Feedback.FeedbackMapper

instance ToSchema FeedbackDTO where
  declareNamedSchema = toSwagger (toDTO defaultConfig defaultProject feedback1)
