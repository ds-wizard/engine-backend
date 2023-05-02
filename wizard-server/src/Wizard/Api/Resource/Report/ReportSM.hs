module Wizard.Api.Resource.Report.ReportSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Model.Report.Report
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Metrics
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance ToSchema Report where
  declareNamedSchema = toSwagger report1

instance ToSchema TotalReport where
  declareNamedSchema = toSwagger report1_total

instance ToSchema ChapterReport where
  declareNamedSchema = toSwagger report1_ch1

instance ToSchema Indication where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema AnsweredIndication where
  declareNamedSchema = toSwaggerWithType "indicationType" answeredAnsweredIndication

instance ToSchema PhasesAnsweredIndication where
  declareNamedSchema = toSwaggerWithType "indicationType" phasesAnsweredIndication

instance ToSchema MetricSummary where
  declareNamedSchema = toSwagger metricSummaryF

phasesAnsweredIndication :: PhasesAnsweredIndication
phasesAnsweredIndication =
  PhasesAnsweredIndication
    { answeredQuestions = 5
    , unansweredQuestions = 1
    }

answeredAnsweredIndication :: AnsweredIndication
answeredAnsweredIndication =
  AnsweredIndication {answeredQuestions = 12, unansweredQuestions = 1}

metricSummaryF :: MetricSummary
metricSummaryF = MetricSummary {metricUuid = metricF.uuid, measure = Just 1.0}
