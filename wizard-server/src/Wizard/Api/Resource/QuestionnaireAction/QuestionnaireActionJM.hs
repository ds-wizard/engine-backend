module Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON QuestionnaireActionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireActionDTO where
  toJSON = genericToJSON jsonOptions
