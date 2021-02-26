module Wizard.Api.Resource.Document.DocumentContextDTO where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Level.Level
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report

data DocumentContextDTO =
  DocumentContextDTO
    { _documentContextDTOUuid :: U.UUID
    , _documentContextDTOConfig :: DocumentContextConfigDTO
    , _documentContextDTOQuestionnaireUuid :: String
    , _documentContextDTOQuestionnaireName :: String
    , _documentContextDTOQuestionnaireReplies :: M.Map String Reply
    , _documentContextDTOQuestionnaireVersions :: [QuestionnaireVersionDTO]
    , _documentContextDTOLevel :: Int
    , _documentContextDTOKnowledgeModel :: KnowledgeModel
    , _documentContextDTOMetrics :: [Metric]
    , _documentContextDTOLevels :: [Level]
    , _documentContextDTOReport :: Report
    , _documentContextDTOPackage :: PackageSimpleDTO
    , _documentContextDTOOrganization :: AppConfigOrganization
    , _documentContextDTOCreatedBy :: Maybe UserDTO
    , _documentContextDTOCreatedAt :: UTCTime
    , _documentContextDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq DocumentContextDTO where
  a == b =
    _documentContextDTOUuid a == _documentContextDTOUuid b &&
    _documentContextDTOConfig a == _documentContextDTOConfig b &&
    _documentContextDTOQuestionnaireUuid a == _documentContextDTOQuestionnaireUuid b &&
    _documentContextDTOQuestionnaireName a == _documentContextDTOQuestionnaireName b &&
    _documentContextDTOQuestionnaireReplies a == _documentContextDTOQuestionnaireReplies b &&
    _documentContextDTOQuestionnaireVersions a == _documentContextDTOQuestionnaireVersions b &&
    _documentContextDTOLevel a == _documentContextDTOLevel b &&
    _documentContextDTOKnowledgeModel a == _documentContextDTOKnowledgeModel b &&
    _documentContextDTOMetrics a == _documentContextDTOMetrics b &&
    _documentContextDTOLevels a == _documentContextDTOLevels b &&
    _documentContextDTOReport a == _documentContextDTOReport b &&
    _documentContextDTOPackage a == _documentContextDTOPackage b &&
    _documentContextDTOOrganization a == _documentContextDTOOrganization b &&
    _documentContextDTOCreatedBy a == _documentContextDTOCreatedBy b

data DocumentContextConfigDTO =
  DocumentContextConfigDTO
    { _documentContextConfigDTOLevelsEnabled :: Bool
    , _documentContextConfigDTOClientUrl :: String
    }
  deriving (Show, Eq, Generic)
