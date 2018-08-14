module Api.Resource.Report.ReportJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Report.ReportDTO

instance ToJSON IndicationDTO where
  toJSON (AnsweredIndicationDTO' event) = toJSON event

instance FromJSON IndicationDTO where
  parseJSON (Object o) = do
    indicationType <- o .: "indicationType"
    case indicationType of
      "AnsweredIndication" -> parseJSON (Object o) >>= \event -> return (AnsweredIndicationDTO' event)
      _ -> fail "One of the references has unsupported indicationType"
  parseJSON _ = mzero

instance ToJSON AnsweredIndicationDTO where
  toJSON AnsweredIndicationDTO {..} =
    object
      [ "indicationType" .= "AnsweredIndication"
      , "answeredQuestions" .= _answeredIndicationDTOAnsweredQuestions
      , "unansweredQuestions" .= _answeredIndicationDTOUnansweredQuestions
      ]

instance FromJSON AnsweredIndicationDTO where
  parseJSON (Object o) = do
    _answeredIndicationDTOAnsweredQuestions <- o .: "answeredQuestions"
    _answeredIndicationDTOUnansweredQuestions <- o .: "unansweredQuestions"
    return AnsweredIndicationDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON MetricSummaryDTO where
  toJSON MetricSummaryDTO {..} =
    object ["metricUuid" .= _metricSummaryDTOMetricUuid, "measure" .= _metricSummaryDTOMeasure]

instance FromJSON MetricSummaryDTO where
  parseJSON (Object o) = do
    _metricSummaryDTOMetricUuid <- o .: "metricUuid"
    _metricSummaryDTOMeasure <- o .: "measure"
    return MetricSummaryDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON ChapterReportDTO where
  toJSON ChapterReportDTO {..} =
    object
      [ "chapterUuid" .= _chapterReportDTOChapterUuid
      , "indications" .= _chapterReportDTOIndications
      , "metrics" .= _chapterReportDTOMetrics
      ]

instance FromJSON ChapterReportDTO where
  parseJSON (Object o) = do
    _chapterReportDTOChapterUuid <- o .: "chapterUuid"
    _chapterReportDTOIndications <- o .: "indications"
    _chapterReportDTOMetrics <- o .: "metrics"
    return ChapterReportDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON ReportDTO where
  toJSON ReportDTO {..} =
    object
      [ "uuid" .= _reportDTOUuid
      , "chapterReports" .= _reportDTOChapterReports
      , "createdAt" .= _reportDTOCreatedAt
      , "updatedAt" .= _reportDTOUpdatedAt
      ]

instance FromJSON ReportDTO where
  parseJSON (Object o) = do
    _reportDTOUuid <- o .: "uuid"
    _reportDTOChapterReports <- o .: "chapterReports"
    _reportDTOCreatedAt <- o .: "createdAt"
    _reportDTOUpdatedAt <- o .: "updatedAt"
    return ReportDTO {..}
  parseJSON _ = mzero
