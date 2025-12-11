module Wizard.Api.Resource.Project.ProjectReplySM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.AesonSM ()
import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.ProjectReplyJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectReplies
import Wizard.Model.Project.ProjectReply
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema Reply where
  declareNamedSchema = toSwagger (fst rQ1Updated)

instance ToSchema ReplyValue where
  declareNamedSchema = toSwagger ((snd rQ1).value)

instance ToSchema IntegrationReplyType where
  declareNamedSchema = toSwagger rQ10IntValue
