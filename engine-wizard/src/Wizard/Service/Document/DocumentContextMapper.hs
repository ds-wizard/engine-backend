module Wizard.Service.Document.DocumentContextMapper where

import Control.Lens ((^.))
import Data.Map as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Level.Level
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Report.Report
import Wizard.Model.User.User
import Wizard.Service.Package.PackageMapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QTN_Mapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper

toDocumentContextDTO :: DocumentContext -> DocumentContextDTO
toDocumentContextDTO dc =
  DocumentContextDTO
    { _documentContextDTOUuid = dc ^. uuid
    , _documentContextDTOConfig = toDocumentContextConfigDTO $ dc ^. config
    , _documentContextDTOQuestionnaireUuid = dc ^. questionnaireUuid
    , _documentContextDTOQuestionnaireName = dc ^. questionnaireName
    , _documentContextDTOQuestionnaireReplies = replies
    , _documentContextDTOQuestionnaireRepliesMap = M.fromList $ (\reply -> (reply ^. path, reply)) <$> replies
    , _documentContextDTOLevel = dc ^. level
    , _documentContextDTOKnowledgeModel = dc ^. knowledgeModel
    , _documentContextDTOMetrics = dc ^. metrics
    , _documentContextDTOLevels = dc ^. levels
    , _documentContextDTOReport = dc ^. report
    , _documentContextDTOPackage = toSimpleDTO (dc ^. package)
    , _documentContextDTOOrganization = dc ^. organization
    , _documentContextDTOCreatedBy = USR_Mapper.toDTO <$> dc ^. createdBy
    , _documentContextDTOCreatedAt = dc ^. createdAt
    , _documentContextDTOUpdatedAt = dc ^. updatedAt
    }
  where
    replies = QTN_Mapper.toReplyDTO <$> dc ^. questionnaireReplies

toDocumentContextConfigDTO :: DocumentContextConfig -> DocumentContextConfigDTO
toDocumentContextConfigDTO config =
  DocumentContextConfigDTO
    { _documentContextConfigDTOLevelsEnabled = config ^. levelsEnabled
    , _documentContextConfigDTOClientUrl = config ^. clientUrl
    }

fromCreateContextDTO ::
     U.UUID
  -> AppConfig
  -> ServerConfig
  -> Questionnaire
  -> Int
  -> KnowledgeModel
  -> [Metric]
  -> [Level]
  -> Report
  -> Package
  -> AppConfigOrganization
  -> Maybe User
  -> UTCTime
  -> DocumentContext
fromCreateContextDTO dmpUuid appConfig serverConfig qtn level km metrics ls report pkg org mCreatedBy now =
  DocumentContext
    { _documentContextUuid = dmpUuid
    , _documentContextConfig =
        DocumentContextConfig
          { _documentContextConfigLevelsEnabled = appConfig ^. questionnaire . levels . enabled
          , _documentContextConfigClientUrl = serverConfig ^. general . clientUrl
          }
    , _documentContextQuestionnaireUuid = U.toString $ qtn ^. uuid
    , _documentContextQuestionnaireName = qtn ^. name
    , _documentContextQuestionnaireReplies = qtn ^. replies
    , _documentContextLevel = level
    , _documentContextKnowledgeModel = km
    , _documentContextMetrics = metrics
    , _documentContextLevels = ls
    , _documentContextReport = report
    , _documentContextPackage = pkg
    , _documentContextOrganization = org
    , _documentContextCreatedBy = mCreatedBy
    , _documentContextCreatedAt = now
    , _documentContextUpdatedAt = now
    }
