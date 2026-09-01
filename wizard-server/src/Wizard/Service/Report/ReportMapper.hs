module Wizard.Service.Report.ReportMapper where

import Data.Aeson (Value)

import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Wizard.Model.Report.Report

toDTO :: ProjectDetailQuestionnaireDTO -> Report -> Maybe Value -> ProjectDetailReportDTO
toDTO project report locale =
  ProjectDetailReportDTO
    { uuid = project.uuid
    , name = project.name
    , visibility = project.visibility
    , sharing = project.sharing
    , knowledgeModelPackage = project.knowledgeModelPackage
    , locale = locale
    , isTemplate = project.isTemplate
    , permissions = project.permissions
    , fileCount = project.fileCount
    , totalReport = report.totalReport
    , chapters = report.chapters
    , chapterReports = report.chapterReports
    , metrics = report.metrics
    }
