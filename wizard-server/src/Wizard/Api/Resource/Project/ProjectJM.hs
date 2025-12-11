module Wizard.Api.Resource.Project.ProjectJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Api.Resource.Project.ProjectReportJM ()
import Wizard.Api.Resource.Project.ProjectSharingJM ()
import Wizard.Api.Resource.Project.ProjectStateJM ()
import Wizard.Api.Resource.Project.ProjectVisibilityJM ()
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON ProjectDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDTO where
  toJSON = genericToJSON jsonOptions
