module Wizard.Service.Document.Context.DocumentContextMapper where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import qualified Shared.DocumentTemplate.Constant.DocumentTemplate as TemplateConstant
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Project.File.ProjectFileSimple
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Model.Project.Version.ProjectVersionList
import Wizard.Model.Report.Report
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper

toDocumentContext
  :: Document
  -> String
  -> Project
  -> Maybe U.UUID
  -> M.Map String Reply
  -> M.Map String [U.UUID]
  -> Maybe ProjectVersion
  -> [ProjectVersionList]
  -> [ProjectFileSimple]
  -> KnowledgeModel
  -> Report
  -> KnowledgeModelPackage
  -> TenantConfigOrganization
  -> Maybe User
  -> Maybe User
  -> [DocumentContextUserPerm]
  -> [DocumentContextUserGroupPerm]
  -> DocumentContext
toDocumentContext doc appClientUrl project phaseUuid replies labels mProjectVersion projectVersionDtos projectFiles km report pkg org mProjectCreatedBy mDocCreatedBy users groups =
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
          { uuid = project.uuid
          , name = project.name
          , description = project.description
          , replies = replies
          , phaseUuid = phaseUuid
          , labels = labels
          , versionUuid = fmap (.uuid) mProjectVersion
          , versions = projectVersionDtos
          , projectTags = project.projectTags
          , files = projectFiles
          , createdBy = USR_Mapper.toDTO <$> mProjectCreatedBy
          , createdAt = project.createdAt
          , updatedAt = project.updatedAt
          }
    , knowledgeModel = km
    , report = report
    , package = toDocumentContextPackage pkg
    , organization = org
    , metamodelVersion = TemplateConstant.documentTemplateMetamodelVersion
    , users = users
    , groups = groups
    }

toDocumentContextPackage :: KnowledgeModelPackage -> DocumentContextPackage
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
