module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Api.Resource.Report.ReportSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Report.Report

instance ToSchema QuestionnaireDetailReportDTO where
  declareNamedSchema =
    toSwagger $
      QuestionnaireDetailReportDTO
        { uuid = questionnaire1.uuid
        , name = questionnaire1.name
        , visibility = questionnaire1.visibility
        , sharing = questionnaire1.sharing
        , knowledgeModelPackageId = questionnaire1.knowledgeModelPackageId
        , isTemplate = questionnaire1.isTemplate
        , migrationUuid = Nothing
        , permissions = [qtn1AlbertEditQtnPermDto]
        , fileCount = 0
        , totalReport = report1.totalReport
        , chapterReports = report1.chapterReports
        , chapters = report1.chapters
        , metrics = report1.metrics
        }
