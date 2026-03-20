module Wizard.Service.Document.Context.DocumentContextMapper where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Coordinate.Util.Coordinate
import qualified Shared.DocumentTemplate.Constant.DocumentTemplate as TemplateConstant
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
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
import WizardLib.Public.Model.Tenant.Config.TenantConfig

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
  -> DocumentTemplate
  -> Report
  -> KnowledgeModelPackage
  -> TenantConfigOrganization
  -> TenantConfigLookAndFeel
  -> Maybe User
  -> Maybe User
  -> [DocumentContextUserPerm]
  -> [DocumentContextUserGroupPerm]
  -> DocumentContext
toDocumentContext doc appClientUrl project phaseUuid replies labels mProjectVersion projectVersionDtos projectFiles km dt report pkg org lookAndFeel mProjectCreatedBy mDocCreatedBy users groups =
  DocumentContext
    { config =
        DocumentContextConfig
          { clientUrl = appClientUrl
          , appTitle = lookAndFeel.appTitle
          , appTitleShort = lookAndFeel.appTitleShort
          , illustrationsColor = lookAndFeel.illustrationsColor
          , primaryColor = lookAndFeel.primaryColor
          , logoUrl = lookAndFeel.logoUrl
          }
    , document =
        DocumentContextDocument
          { uuid = doc.uuid
          , name = doc.name
          , documentTemplateId = buildCoordinate dt.organizationId dt.templateId dt.version
          , formatUuid = doc.formatUuid
          , createdBy = toDocumentContextUser <$> mDocCreatedBy
          , createdAt = doc.createdAt
          }
    , project =
        DocumentContextProject
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
          , createdBy = toDocumentContextUser <$> mProjectCreatedBy
          , createdAt = project.createdAt
          , updatedAt = project.updatedAt
          }
    , knowledgeModel = km
    , report = report
    , knowledgeModelPackage = toDocumentContextPackage pkg
    , organization = org
    , metamodelVersion = TemplateConstant.documentTemplateMetamodelVersion
    , users = users
    , groups = groups
    }

toDocumentContextPackage :: KnowledgeModelPackage -> DocumentContextPackage
toDocumentContextPackage pkg =
  let dto = toSimpleDTO pkg
   in DocumentContextPackage
        { uuid = pkg.uuid
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

toDocumentContextUser :: User -> DocumentContextUser
toDocumentContextUser user =
  DocumentContextUser
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , affiliation = user.affiliation
    , active = user.active
    , imageUrl = user.imageUrl
    , createdAt = user.createdAt
    , updatedAt = user.updatedAt
    }
