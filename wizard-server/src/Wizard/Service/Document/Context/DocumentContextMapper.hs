module Wizard.Service.Document.Context.DocumentContextMapper where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireFileSimple
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Questionnaire.QuestionnaireVersionList
import Wizard.Model.Report.Report
import Wizard.Model.User.User
import Wizard.Service.Package.PackageMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper
import qualified WizardLib.DocumentTemplate.Constant.DocumentTemplate as TemplateConstant
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.Public.Model.Tenant.Config.TenantConfig

toDocumentContext
  :: Document
  -> String
  -> Questionnaire
  -> Maybe U.UUID
  -> M.Map String Reply
  -> M.Map String [U.UUID]
  -> Maybe QuestionnaireVersion
  -> [QuestionnaireVersionList]
  -> [QuestionnaireFileSimple]
  -> KnowledgeModel
  -> Report
  -> Package
  -> TenantConfigOrganization
  -> Maybe User
  -> Maybe User
  -> [DocumentContextUserPerm]
  -> [DocumentContextUserGroupPerm]
  -> DocumentContext
toDocumentContext doc appClientUrl qtn phaseUuid replies labels mQtnVersion qtnVersionDtos qtnFiles km report pkg org mQtnCreatedBy mDocCreatedBy users groups =
  DocumentContext
    { config = DocumentContextConfig {clientUrl = appClientUrl}
    , document =
        DocumentContextDocument
          { uuid = doc.uuid
          , name = doc.name
          , documentTemplateId = doc.documentTemplateId
          , formatUuid = doc.formatUuid
          , createdBy = USR_Mapper.toDTO <$> mDocCreatedBy
          , createdAt = doc.createdAt
          }
    , questionnaire =
        DocumentContextQuestionnaire
          { uuid = qtn.uuid
          , name = qtn.name
          , description = qtn.description
          , replies = replies
          , phaseUuid = phaseUuid
          , labels = labels
          , versionUuid = fmap (.uuid) mQtnVersion
          , versions = qtnVersionDtos
          , projectTags = qtn.projectTags
          , files = qtnFiles
          , createdBy = USR_Mapper.toDTO <$> mQtnCreatedBy
          , createdAt = qtn.createdAt
          , updatedAt = qtn.updatedAt
          }
    , knowledgeModel = km
    , report = report
    , package = toDocumentContextPackage pkg
    , organization = org
    , metamodelVersion = TemplateConstant.documentTemplateMetamodelVersion
    , users = users
    , groups = groups
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
        , organization = dto.organization
        , createdAt = dto.createdAt
        }
