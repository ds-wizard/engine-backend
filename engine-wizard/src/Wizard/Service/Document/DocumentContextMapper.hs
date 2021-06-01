module Wizard.Service.Document.DocumentContextMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Level.Level
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Report.Report
import Wizard.Model.User.User
import Wizard.Service.Package.PackageMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper

toDocumentContext ::
     U.UUID
  -> AppConfig
  -> ServerConfig
  -> Questionnaire
  -> QuestionnaireContent
  -> Maybe U.UUID
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
  -> DocumentContext
toDocumentContext dmpUuid appConfig serverConfig qtn qtnCtn qtnVersion qtnVersionDtos level km metrics ls report pkg org mCreatedBy now =
  DocumentContext
    { _documentContextUuid = dmpUuid
    , _documentContextConfig =
        DocumentContextConfig
          { _documentContextConfigLevelsEnabled = appConfig ^. questionnaire . levels . enabled
          , _documentContextConfigClientUrl = serverConfig ^. general . clientUrl
          }
    , _documentContextQuestionnaireUuid = U.toString $ qtn ^. uuid
    , _documentContextQuestionnaireName = qtn ^. name
    , _documentContextQuestionnaireReplies = qtnCtn ^. replies
    , _documentContextQuestionnaireVersion = qtnVersion
    , _documentContextQuestionnaireVersions = qtnVersionDtos
    , _documentContextLevel = level
    , _documentContextKnowledgeModel = km
    , _documentContextMetrics = metrics
    , _documentContextLevels = ls
    , _documentContextReport = report
    , _documentContextPackage = toSimpleDTO pkg
    , _documentContextOrganization = org
    , _documentContextCreatedBy = USR_Mapper.toDTO <$> mCreatedBy
    , _documentContextCreatedAt = now
    , _documentContextUpdatedAt = now
    }
