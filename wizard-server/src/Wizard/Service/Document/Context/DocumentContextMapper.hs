module Wizard.Service.Document.Context.DocumentContextMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Package.PackageSimpleDTO
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
import qualified WizardLib.DocumentTemplate.Constant.DocumentTemplate as TemplateConstant
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.Package

toDocumentContext
  :: U.UUID
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
    { uuid = docUuid
    , config = DocumentContextConfig {clientUrl = appClientUrl}
    , questionnaireUuid = U.toString $ qtn.uuid
    , questionnaireName = qtn.name
    , questionnaireDescription = qtn.description
    , questionnaireReplies = qtnCtn.replies
    , questionnaireVersion = qtnVersion
    , questionnaireVersions = qtnVersionDtos
    , questionnaireProjectTags = qtnProjectTags
    , phaseUuid = mPhase
    , knowledgeModel = km
    , report = report
    , package = toDocumentContextPackage pkg
    , organization = org
    , documentTemplateMetamodelVersion = TemplateConstant.documentTemplateMetamodelVersion
    , createdBy = USR_Mapper.toDTO <$> mCreatedBy
    , createdAt = now
    , updatedAt = now
    }

toDocumentContextPackage :: Package -> DocumentContextPackage
toDocumentContextPackage pkg =
  let dto = toSimpleDTO pkg
   in DocumentContextPackage
        { pId = dto.pId
        , name = dto.name
        , organizationId = dto.organizationId
        , kmId = pkg.kmId
        , version = dto.version
        , versions = []
        , remoteLatestVersion = dto.remoteLatestVersion
        , description = dto.description
        , state = dto.state
        , organization = dto.organization
        , createdAt = dto.createdAt
        }
