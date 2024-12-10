module Wizard.Api.Resource.Branch.Event.SetRepliesSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Branch.Event.SetRepliesDTO
import Wizard.Api.Resource.Branch.Event.SetRepliesJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Database.Migration.Development.Branch.Data.BranchEvents

instance ToSchema SetRepliesDTO where
  declareNamedSchema = toSwagger setRepliesDTO
