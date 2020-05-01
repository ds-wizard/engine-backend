module Wizard.Api.Resource.Feedback.FeedbackCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Api.Resource.Feedback.FeedbackCreateJM ()
import Wizard.Database.Migration.Development.Feedback.Data.Feedbacks

instance ToSchema FeedbackCreateDTO where
  declareNamedSchema = simpleToSchema feedback1Create
