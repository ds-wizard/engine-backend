module Wizard.Api.Resource.Document.DocumentContextDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Wizard.Api.Resource.Level.LevelDTO
import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Api.Resource.Report.ReportDTO
import Wizard.Api.Resource.User.UserDTO

data DocumentContextDTO =
  DocumentContextDTO
    { _documentContextDTOUuid :: U.UUID
    , _documentContextDTOConfig :: DocumentContextConfigDTO
    , _documentContextDTOQuestionnaireUuid :: String
    , _documentContextDTOQuestionnaireName :: String
    , _documentContextDTOQuestionnaireReplies :: [ReplyDTO]
    , _documentContextDTOLevel :: Int
    , _documentContextDTOKnowledgeModel :: KnowledgeModelDTO
    , _documentContextDTOMetrics :: [MetricDTO]
    , _documentContextDTOLevels :: [LevelDTO]
    , _documentContextDTOReport :: ReportDTO
    , _documentContextDTOPackage :: PackageSimpleDTO
    , _documentContextDTOOrganization :: OrganizationDTO
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
    }
  deriving (Show, Eq, Generic)
