module Wizard.Api.Resource.Report.ReportJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Report.Report
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()

-- --------------------------------------------------------------------
instance FromJSON Report where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Report where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance FromJSON TotalReport where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TotalReport where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance FromJSON ChapterReport where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ChapterReport where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance FromJSON MetricSummary where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MetricSummary where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance ToJSON Indication where
  toJSON = toSumJSONWithTypeField "indicationType" ""

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
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AnsweredIndication where
  toJSON = genericToJSON jsonOptions

-- --------------------------------------------------------------------
instance FromJSON PhasesAnsweredIndication where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PhasesAnsweredIndication where
  toJSON = genericToJSON jsonOptions
