module Wizard.Service.Document.Context.DocumentContextMapper where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireCommentList
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Report.Report
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Service.Package.PackageMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper
import qualified WizardLib.DocumentTemplate.Constant.DocumentTemplate as TemplateConstant
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.Package

toDocumentContext
  :: Document
  -> String
  -> Questionnaire
  -> QuestionnaireContent
  -> Maybe U.UUID
  -> [QuestionnaireVersionDTO]
  -> M.Map String [QuestionnaireCommentThreadList]
  -> KnowledgeModel
  -> Report
  -> Package
  -> TenantConfigOrganization
  -> Maybe User
  -> Maybe User
  -> [DocumentContextUserPerm]
  -> [DocumentContextUserGroupPerm]
  -> DocumentContext
toDocumentContext doc appClientUrl qtn qtnCtn qtnVersion qtnVersionDtos qtnComments km report pkg org mQtnCreatedBy mDocCreatedBy users groups =
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
          , replies = qtnCtn.replies
          , phaseUuid = qtnCtn.phaseUuid
          , labels = qtnCtn.labels
          , comments = qtnComments
          , versionUuid = qtnVersion
          , versions = qtnVersionDtos
          , projectTags = qtn.projectTags
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
