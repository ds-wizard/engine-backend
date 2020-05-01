module Wizard.Api.Resource.Report.ReportJM where

import Control.Monad
import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Report.ReportDTO

-- --------------------------------------------------------------------
instance FromJSON ReportDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ReportDTO where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
instance FromJSON TotalReportDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TotalReportDTO where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
instance FromJSON ChapterReportDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ChapterReportDTO where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
instance FromJSON MetricSummaryDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MetricSummaryDTO where
  toJSON = genericToJSON simpleOptions

-- --------------------------------------------------------------------
instance ToJSON IndicationDTO where
  toJSON = toSumJSON

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
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AnsweredIndicationDTO where
  toJSON = simpleToJSON' "_answeredIndicationDTO" "indicationType"

-- --------------------------------------------------------------------
instance FromJSON LevelsAnsweredIndicationDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON LevelsAnsweredIndicationDTO where
  toJSON = simpleToJSON' "_levelsAnsweredIndicationDTO" "indicationType"
