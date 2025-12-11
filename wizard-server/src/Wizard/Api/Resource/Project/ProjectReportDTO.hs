module Wizard.Api.Resource.Project.ProjectReportDTO where

import GHC.Generics

import Wizard.Model.Report.Report

data ProjectReportDTO = ProjectReportDTO
  { indications :: [Indication]
  }
  deriving (Show, Eq, Generic)
