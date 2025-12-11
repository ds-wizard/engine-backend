module Wizard.Api.Resource.Project.ProjectReportJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.ProjectReportDTO
import Wizard.Api.Resource.Report.ReportJM ()

instance FromJSON ProjectReportDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectReportDTO where
  toJSON = genericToJSON jsonOptions
