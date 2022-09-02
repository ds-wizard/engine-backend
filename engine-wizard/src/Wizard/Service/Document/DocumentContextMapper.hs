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
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Report.Report
import Wizard.Model.User.User
import Wizard.Service.Package.PackageMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper

toDocumentContext ::
     U.UUID
  -> ServerConfig
  -> String
  -> Questionnaire
  -> QuestionnaireContent
  -> Maybe U.UUID
  -> [QuestionnaireVersionDTO]
  -> [String]
  -> Maybe U.UUID
  -> KnowledgeModel
  -> Report
  -> Package
  -> AppConfigOrganization
  -> Maybe User
  -> UTCTime
  -> DocumentContext
toDocumentContext docUuid serverConfig appClientUrl qtn qtnCtn qtnVersion qtnVersionDtos qtnProjectTags mPhase km report pkg org mCreatedBy now =
  DocumentContext
    { _documentContextUuid = docUuid
    , _documentContextConfig = DocumentContextConfig {_documentContextConfigClientUrl = appClientUrl}
    , _documentContextQuestionnaireUuid = U.toString $ qtn ^. uuid
    , _documentContextQuestionnaireName = qtn ^. name
    , _documentContextQuestionnaireDescription = qtn ^. description
    , _documentContextQuestionnaireReplies = qtnCtn ^. replies
    , _documentContextQuestionnaireVersion = qtnVersion
    , _documentContextQuestionnaireVersions = qtnVersionDtos
    , _documentContextQuestionnaireProjectTags = qtnProjectTags
    , _documentContextPhaseUuid = mPhase
    , _documentContextKnowledgeModel = km
    , _documentContextReport = report
    , _documentContextPackage = toDocumentContextPackage pkg
    , _documentContextOrganization = org
    , _documentContextCreatedBy = USR_Mapper.toDTO <$> mCreatedBy
    , _documentContextCreatedAt = now
    , _documentContextUpdatedAt = now
    }

toDocumentContextPackage :: Package -> DocumentContextPackage
toDocumentContextPackage pkg =
  let dto = toSimpleDTO pkg
   in DocumentContextPackage
        { _documentContextPackagePId = dto ^. pId
        , _documentContextPackageName = dto ^. name
        , _documentContextPackageOrganizationId = dto ^. organizationId
        , _documentContextPackageKmId = pkg ^. kmId
        , _documentContextPackageVersion = dto ^. version
        , _documentContextPackageVersions = []
        , _documentContextPackageRemoteLatestVersion = dto ^. remoteLatestVersion
        , _documentContextPackageDescription = dto ^. description
        , _documentContextPackageState = dto ^. state
        , _documentContextPackageOrganization = dto ^. organization
        , _documentContextPackageCreatedAt = dto ^. createdAt
        }
