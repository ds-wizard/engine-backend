module Wizard.Api.Resource.Report.ReportJM where

import Control.Monad
import Data.Aeson

import Wizard.Api.Resource.Report.ReportDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON, simpleToJSON')

-- --------------------------------------------------------------------
instance FromJSON ReportDTO where
  parseJSON = simpleParseJSON "_reportDTO"

instance ToJSON ReportDTO where
  toJSON = simpleToJSON "_reportDTO"

-- --------------------------------------------------------------------
instance FromJSON ChapterReportDTO where
  parseJSON = simpleParseJSON "_chapterReportDTO"

instance ToJSON ChapterReportDTO where
  toJSON = simpleToJSON "_chapterReportDTO"

-- --------------------------------------------------------------------
instance FromJSON MetricSummaryDTO where
  parseJSON = simpleParseJSON "_metricSummaryDTO"

instance ToJSON MetricSummaryDTO where
  toJSON = simpleToJSON "_metricSummaryDTO"

-- --------------------------------------------------------------------
instance ToJSON IndicationDTO where
  toJSON (AnsweredIndicationDTO' event) = toJSON event
  toJSON (LevelsAnsweredIndicationDTO' event) = toJSON event

instance FromJSON IndicationDTO where
  parseJSON (Object o) = do
    indicationType <- o .: "indicationType"
    case indicationType of
      "AnsweredIndication" -> parseJSON (Object o) >>= \event -> return (AnsweredIndicationDTO' event)
      "LevelsAnsweredIndication" -> parseJSON (Object o) >>= \event -> return (LevelsAnsweredIndicationDTO' event)
      _ -> fail "One of the references has unsupported indicationType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON AnsweredIndicationDTO where
  parseJSON = simpleParseJSON "_answeredIndicationDTO"

instance ToJSON AnsweredIndicationDTO where
  toJSON = simpleToJSON' "indicationType" "_answeredIndicationDTO"

-- --------------------------------------------------------------------
instance FromJSON LevelsAnsweredIndicationDTO where
  parseJSON = simpleParseJSON "_levelsAnsweredIndicationDTO"

instance ToJSON LevelsAnsweredIndicationDTO where
  toJSON = simpleToJSON' "indicationType" "_levelsAnsweredIndicationDTO"
