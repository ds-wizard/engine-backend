module Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.File.ProjectFileSimpleJM ()
import Wizard.Api.Resource.Project.ProjectReplyJM ()
import Wizard.Api.Resource.Project.ProjectSharingJM ()
import Wizard.Api.Resource.Project.ProjectVisibilityJM ()

instance FromJSON ProjectDetailQuestionnaireDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailQuestionnaireDTO where
  toJSON = genericToJSON jsonOptions
