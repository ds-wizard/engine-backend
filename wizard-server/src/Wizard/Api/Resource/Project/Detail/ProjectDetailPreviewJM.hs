module Wizard.Api.Resource.Project.Detail.ProjectDetailPreviewJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Api.Resource.Project.ProjectSharingJM ()
import Wizard.Api.Resource.Project.ProjectVisibilityJM ()
import Wizard.Model.Project.Detail.ProjectDetailPreview

instance FromJSON ProjectDetailPreview where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailPreview where
  toJSON = genericToJSON jsonOptions
