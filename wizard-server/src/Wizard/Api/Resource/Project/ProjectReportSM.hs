module Wizard.Api.Resource.Project.ProjectReportSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.ProjectReportDTO
import Wizard.Api.Resource.Project.ProjectReportJM ()
import Wizard.Api.Resource.Report.ReportSM ()
import Wizard.Database.Migration.Development.Report.Data.Reports

instance ToSchema ProjectReportDTO where
  declareNamedSchema = toSwagger projectReport
