module Wizard.Api.Resource.Project.Detail.ProjectDetailSettingsSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import qualified Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper as DocumentTemplateMapper
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermSM ()
import Wizard.Api.Resource.Project.Comment.ProjectCommentThreadListSM ()
import Wizard.Api.Resource.Project.Detail.ProjectDetailSettingsJM ()
import Wizard.Api.Resource.Project.ProjectReplySM ()
import Wizard.Api.Resource.Project.ProjectSharingSM ()
import Wizard.Api.Resource.Project.ProjectStateSM ()
import Wizard.Api.Resource.Project.ProjectVisibilitySM ()
import Wizard.Api.Resource.Project.Version.ProjectVersionListSM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Project.Detail.ProjectDetailSettings
import Wizard.Model.Project.Project
import qualified Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as PackageMapper

instance ToSchema ProjectDetailSettings where
  declareNamedSchema =
    toSwagger $
      ProjectDetailSettings
        { uuid = project1.uuid
        , name = project1.name
        , description = project1.description
        , visibility = project1.visibility
        , sharing = project1.sharing
        , selectedQuestionTagUuids = project1.selectedQuestionTagUuids
        , isTemplate = project1.isTemplate
        , migrationUuid = Nothing
        , permissions = [project1AlbertEditProjectPermDto]
        , projectTags = project1.projectTags
        , knowledgeModelPackageId = netherlandsKmPackageV2.pId
        , knowledgeModelPackage = PackageMapper.toSimpleDTO netherlandsKmPackageV2
        , knowledgeModelTags = [tagDataScience]
        , documentTemplate = Just $ DocumentTemplateMapper.toDTO wizardDocumentTemplate wizardDocumentTemplateFormats
        , documentTemplateState = Just DefaultDocumentTemplateState
        , documentTemplatePhase = Just DraftDocumentTemplatePhase
        , formatUuid = Just . u' $ "ae3b9e68-e09e-4ad7-b476-67ab5626e873"
        , fileCount = 0
        }
