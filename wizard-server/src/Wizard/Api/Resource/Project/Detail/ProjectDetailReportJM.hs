module Wizard.Api.Resource.Project.Detail.ProjectDetailReportJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Wizard.Api.Resource.Project.ProjectSharingJM ()
import Wizard.Api.Resource.Project.ProjectVisibilityJM ()
import Wizard.Api.Resource.Report.ReportJM ()

instance FromJSON ProjectDetailReportDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailReportDTO where
  toJSON = genericToJSON jsonOptions
