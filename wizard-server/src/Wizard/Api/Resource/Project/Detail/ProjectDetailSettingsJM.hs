module Wizard.Api.Resource.Project.Detail.ProjectDetailSettingsJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Api.Resource.Project.ProjectReplyJM ()
import Wizard.Api.Resource.Project.ProjectSharingJM ()
import Wizard.Api.Resource.Project.ProjectStateJM ()
import Wizard.Api.Resource.Project.ProjectVisibilityJM ()
import Wizard.Model.Project.Detail.ProjectDetailSettings

instance FromJSON ProjectDetailSettings where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailSettings where
  toJSON = genericToJSON jsonOptions
