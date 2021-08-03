module Wizard.Api.Resource.Report.ReportJM where

import Control.Monad
import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Report.Report

-- --------------------------------------------------------------------
instance FromJSON Report where
  parseJSON = simpleParseJSON "_report"

instance ToJSON Report where
  toJSON = simpleToJSON "_report"

-- --------------------------------------------------------------------
instance FromJSON TotalReport where
  parseJSON = simpleParseJSON "_totalReport"

instance ToJSON TotalReport where
  toJSON = simpleToJSON "_totalReport"

-- --------------------------------------------------------------------
instance FromJSON ChapterReport where
  parseJSON = simpleParseJSON "_chapterReport"

instance ToJSON ChapterReport where
  toJSON = simpleToJSON "_chapterReport"

-- --------------------------------------------------------------------
instance FromJSON MetricSummary where
  parseJSON = simpleParseJSON "_metricSummary"

instance ToJSON MetricSummary where
  toJSON = simpleToJSON "_metricSummary"

-- --------------------------------------------------------------------
instance ToJSON Indication where
  toJSON = toSumJSON

instance FromJSON Indication where
  parseJSON (Object o) = do
    indicationType <- o .: "indicationType"
    case indicationType of
      "AnsweredIndication" -> parseJSON (Object o) >>= \event -> return (AnsweredIndication' event)
      "PhasesAnsweredIndication" -> parseJSON (Object o) >>= \event -> return (PhasesAnsweredIndication' event)
      _ -> fail "One of the references has unsupported indicationType"
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance FromJSON AnsweredIndication where
  parseJSON = simpleParseJSON "_answeredIndication"

instance ToJSON AnsweredIndication where
  toJSON = simpleToJSON' "_answeredIndication" "indicationType"

-- --------------------------------------------------------------------
instance FromJSON PhasesAnsweredIndication where
  parseJSON = simpleParseJSON "_phasesAnsweredIndication"

instance ToJSON PhasesAnsweredIndication where
  toJSON = simpleToJSON' "_phasesAnsweredIndication" "indicationType"
