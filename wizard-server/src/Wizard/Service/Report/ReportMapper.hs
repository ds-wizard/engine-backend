module Wizard.Service.Report.ReportMapper where

import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportDTO
import Wizard.Model.Report.Report

toDTO :: QuestionnaireDetailQuestionnaireDTO -> Report -> QuestionnaireDetailReportDTO
toDTO qtn report =
  QuestionnaireDetailReportDTO
    { uuid = qtn.uuid
    , name = qtn.name
    , visibility = qtn.visibility
    , sharing = qtn.sharing
    , knowledgeModelPackageId = qtn.knowledgeModelPackageId
    , isTemplate = qtn.isTemplate
    , permissions = qtn.permissions
    , migrationUuid = qtn.migrationUuid
    , fileCount = qtn.fileCount
    , totalReport = report.totalReport
    , chapters = report.chapters
    , chapterReports = report.chapterReports
    , metrics = report.metrics
    }
