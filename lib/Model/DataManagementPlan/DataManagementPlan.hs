module Model.DataManagementPlan.DataManagementPlan where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.KnowledgeModel.KnowledgeModel
import Model.Report.Report

data DataManagementPlanFormat
  = JSON
  | HTML
  | PDF
  | LaTeX
  | Docx
  | ODT
  | Markdown
  | RTF
  | RST
  | AsciiDoc
  | DokuWiki
  | MediaWiki
  | EPUB2
  | EPUB3
  deriving (Show, Eq, Enum, Bounded, Generic)

data DataManagementPlan = DataManagementPlan
  { _dataManagementPlanUuid :: U.UUID
  , _dataManagementPlanQuestionnaireUuid :: String
  , _dataManagementPlanLevel :: Int
  , _dataManagementPlanFilledKnowledgeModel :: FilledKnowledgeModel
  , _dataManagementPlanMetrics :: [Metric]
  , _dataManagementPlanReport :: Report
  , _dataManagementPlanCreatedAt :: UTCTime
  , _dataManagementPlanUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq DataManagementPlan where
  a == b =
    _dataManagementPlanUuid a == _dataManagementPlanUuid b &&
    _dataManagementPlanQuestionnaireUuid a == _dataManagementPlanQuestionnaireUuid b &&
    _dataManagementPlanLevel a == _dataManagementPlanLevel b &&
    _dataManagementPlanFilledKnowledgeModel a == _dataManagementPlanFilledKnowledgeModel b &&
    _dataManagementPlanMetrics a == _dataManagementPlanMetrics b &&
    _dataManagementPlanReport a == _dataManagementPlanReport b
