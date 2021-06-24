module Wizard.Model.Document.DocumentContext where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report

data DocumentContext =
  DocumentContext
    { _documentContextUuid :: U.UUID
    , _documentContextConfig :: DocumentContextConfig
    , _documentContextQuestionnaireUuid :: String
    , _documentContextQuestionnaireName :: String
    , _documentContextQuestionnaireReplies :: M.Map String Reply
    , _documentContextQuestionnaireVersion :: Maybe U.UUID
    , _documentContextQuestionnaireVersions :: [QuestionnaireVersionDTO]
    , _documentContextPhaseUuid :: Maybe U.UUID
    , _documentContextKnowledgeModel :: KnowledgeModel
    , _documentContextReport :: Report
    , _documentContextPackage :: PackageSimpleDTO
    , _documentContextOrganization :: AppConfigOrganization
    , _documentContextCreatedBy :: Maybe UserDTO
    , _documentContextCreatedAt :: UTCTime
    , _documentContextUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq DocumentContext where
  a == b =
    _documentContextUuid a == _documentContextUuid b &&
    _documentContextConfig a == _documentContextConfig b &&
    _documentContextQuestionnaireUuid a == _documentContextQuestionnaireUuid b &&
    _documentContextQuestionnaireName a == _documentContextQuestionnaireName b &&
    _documentContextQuestionnaireReplies a == _documentContextQuestionnaireReplies b &&
    _documentContextQuestionnaireVersion a == _documentContextQuestionnaireVersion b &&
    _documentContextQuestionnaireVersions a == _documentContextQuestionnaireVersions b &&
    _documentContextPhaseUuid a == _documentContextPhaseUuid b &&
    _documentContextKnowledgeModel a == _documentContextKnowledgeModel b &&
    _documentContextReport a == _documentContextReport b &&
    _documentContextPackage a == _documentContextPackage b &&
    _documentContextOrganization a == _documentContextOrganization b &&
    _documentContextCreatedBy a == _documentContextCreatedBy b

data DocumentContextConfig =
  DocumentContextConfig
    { _documentContextConfigClientUrl :: String
    }
  deriving (Show, Eq, Generic)
