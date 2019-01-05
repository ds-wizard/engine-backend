module Model.DataManagementPlan.DataManagementPlan where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.KnowledgeModel.KnowledgeModel
import Model.Level.Level
import Model.Organization.Organization
import Model.Package.Package
import Model.Report.Report
import Model.User.User

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
  , _dataManagementPlanLevels :: [Level]
  , _dataManagementPlanReport :: Report
  , _dataManagementPlanPackage :: Package
  , _dataManagementPlanOrganization :: Organization
  , _dataManagementPlanCreatedBy :: Maybe User
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
    _dataManagementPlanLevels a == _dataManagementPlanLevels b &&
    _dataManagementPlanReport a == _dataManagementPlanReport b &&
    _dataManagementPlanPackage a == _dataManagementPlanPackage b &&
    _dataManagementPlanOrganization a == _dataManagementPlanOrganization b &&
    _dataManagementPlanCreatedBy a == _dataManagementPlanCreatedBy b
