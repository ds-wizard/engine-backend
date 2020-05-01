module Wizard.Api.Resource.Feedback.FeedbackSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Feedback.FeedbackDTO
import Wizard.Api.Resource.Feedback.FeedbackJM ()
import Wizard.Database.Migration.Development.Feedback.Data.Feedbacks
import Wizard.Service.Feedback.FeedbackMapper

instance ToSchema FeedbackDTO where
  declareNamedSchema = simpleToSchema (toDTO feedback1 "https://github.com/myorg/myrepo/issues/187")
