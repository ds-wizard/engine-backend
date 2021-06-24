module Wizard.Api.Resource.Report.ReportSM where

import Control.Lens ((^.))
import Data.Swagger

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Metrics
import Shared.Util.Swagger
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Model.Report.Report

instance ToSchema Report where
  declareNamedSchema = simpleToSchema' "_report" report1

instance ToSchema TotalReport where
  declareNamedSchema = simpleToSchema' "_totalReport" report1_total

instance ToSchema ChapterReport where
  declareNamedSchema = simpleToSchema' "_chapterReport" report1_ch1

instance ToSchema Indication where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema AnsweredIndication where
  declareNamedSchema = simpleToSchema'' "_answeredIndication" "indicationType" answeredAnsweredIndication

instance ToSchema LevelsAnsweredIndication where
  declareNamedSchema = simpleToSchema'' "_levelsAnsweredIndication" "indicationType" levelsAnsweredIndication

instance ToSchema MetricSummary where
  declareNamedSchema = simpleToSchema' "_metricSummary" metricSummaryF

levelsAnsweredIndication :: LevelsAnsweredIndication
levelsAnsweredIndication =
  LevelsAnsweredIndication
    {_levelsAnsweredIndicationAnsweredQuestions = 5, _levelsAnsweredIndicationUnansweredQuestions = 1}

answeredAnsweredIndication :: AnsweredIndication
answeredAnsweredIndication =
  AnsweredIndication {_answeredIndicationAnsweredQuestions = 12, _answeredIndicationUnansweredQuestions = 1}

metricSummaryF :: MetricSummary
metricSummaryF = MetricSummary {_metricSummaryMetricUuid = metricF ^. uuid, _metricSummaryMeasure = Just 1.0}
