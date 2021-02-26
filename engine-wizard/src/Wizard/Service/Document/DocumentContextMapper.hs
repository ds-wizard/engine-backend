module Wizard.Service.Document.DocumentContextMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Level.Level
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Report.Report
import Wizard.Model.User.User
import Wizard.Service.Package.PackageMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper

toDocumentContextDTO ::
     U.UUID
  -> AppConfig
  -> ServerConfig
  -> Questionnaire
  -> QuestionnaireContent
  -> [QuestionnaireVersionDTO]
  -> Int
  -> KnowledgeModel
  -> [Metric]
  -> [Level]
  -> Report
  -> Package
  -> AppConfigOrganization
  -> Maybe User
  -> UTCTime
  -> DocumentContextDTO
toDocumentContextDTO dmpUuid appConfig serverConfig qtn qtnCtn qtnVersionDtos level km metrics ls report pkg org mCreatedBy now =
  DocumentContextDTO
    { _documentContextDTOUuid = dmpUuid
    , _documentContextDTOConfig =
        DocumentContextConfigDTO
          { _documentContextConfigDTOLevelsEnabled = appConfig ^. questionnaire . levels . enabled
          , _documentContextConfigDTOClientUrl = serverConfig ^. general . clientUrl
          }
    , _documentContextDTOQuestionnaireUuid = U.toString $ qtn ^. uuid
    , _documentContextDTOQuestionnaireName = qtn ^. name
    , _documentContextDTOQuestionnaireReplies = qtnCtn ^. replies
    , _documentContextDTOQuestionnaireVersions = qtnVersionDtos
    , _documentContextDTOLevel = level
    , _documentContextDTOKnowledgeModel = km
    , _documentContextDTOMetrics = metrics
    , _documentContextDTOLevels = ls
    , _documentContextDTOReport = report
    , _documentContextDTOPackage = toSimpleDTO pkg
    , _documentContextDTOOrganization = org
    , _documentContextDTOCreatedBy = USR_Mapper.toDTO <$> mCreatedBy
    , _documentContextDTOCreatedAt = now
    , _documentContextDTOUpdatedAt = now
    }
