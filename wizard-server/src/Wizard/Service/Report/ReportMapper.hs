module Wizard.Service.Report.ReportMapper where

import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Wizard.Model.Report.Report

toDTO :: ProjectDetailQuestionnaireDTO -> Report -> ProjectDetailReportDTO
toDTO project report =
  ProjectDetailReportDTO
    { uuid = project.uuid
    , name = project.name
    , visibility = project.visibility
    , sharing = project.sharing
    , knowledgeModelPackageId = project.knowledgeModelPackageId
    , isTemplate = project.isTemplate
    , permissions = project.permissions
    , migrationUuid = project.migrationUuid
    , fileCount = project.fileCount
    , totalReport = report.totalReport
    , chapters = report.chapters
    , chapterReports = report.chapterReports
    , metrics = report.metrics
    }
