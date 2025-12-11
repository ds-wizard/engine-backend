module Wizard.Database.Migration.Development.Project.Data.ProjectMigrations where

import qualified Data.Map.Strict as M

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Project.Migration.ProjectMigration
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Project.ProjectMapper

projectMigration :: ProjectMigration
projectMigration =
  ProjectMigration
    { oldProjectUuid = projectMigrationDto.oldProject.uuid
    , newProjectUuid = projectMigrationDto.newProject.uuid
    , resolvedQuestionUuids = [question2.uuid]
    , tenantUuid = defaultTenant.uuid
    }

projectMigrationDto :: ProjectMigrationDTO
projectMigrationDto =
  ProjectMigrationDTO
    { oldProject =
        toDetailProjectDTO
          (toDetailQuestionnaire project4 (Just project4Upgraded.uuid) [] 0 0)
          M.empty
          M.empty
          km1Netherlands
          project4Ctn.phaseUuid
          project4Ctn.replies
          project4Ctn.labels
    , newProject =
        toDetailProjectDTO
          (toDetailQuestionnaire project4Upgraded Nothing [] 0 0)
          M.empty
          M.empty
          km1NetherlandsV2
          project4Ctn.phaseUuid
          project4Ctn.replies
          project4Ctn.labels
    , resolvedQuestionUuids = [question2.uuid]
    , tenantUuid = defaultTenant.uuid
    }

projectMigrationVisibleViewDto :: ProjectMigrationDTO
projectMigrationVisibleViewDto =
  ProjectMigrationDTO
    { oldProject =
        toDetailProjectDTO
          (toDetailQuestionnaire project4VisibleView (Just project4Upgraded.uuid) [] 0 0)
          M.empty
          M.empty
          km1Netherlands
          project4Ctn.phaseUuid
          project4Ctn.replies
          project4Ctn.labels
    , newProject =
        toDetailProjectDTO
          (toDetailQuestionnaire project4VisibleViewUpgraded Nothing [] 0 0)
          M.empty
          M.empty
          km1NetherlandsV2
          project4Ctn.phaseUuid
          project4Ctn.replies
          project4Ctn.labels
    , resolvedQuestionUuids = projectMigrationDto.resolvedQuestionUuids
    , tenantUuid = defaultTenant.uuid
    }

projectMigrationVisibleEditDto :: ProjectMigrationDTO
projectMigrationVisibleEditDto =
  ProjectMigrationDTO
    { oldProject =
        toDetailProjectDTO
          (toDetailQuestionnaire project4VisibleEdit (Just project4Upgraded.uuid) [] 0 0)
          M.empty
          M.empty
          km1Netherlands
          project4Ctn.phaseUuid
          project4Ctn.replies
          project4Ctn.labels
    , newProject =
        toDetailProjectDTO
          (toDetailQuestionnaire project4VisibleEditUpgraded Nothing [] 0 0)
          M.empty
          M.empty
          km1NetherlandsV2
          project4Ctn.phaseUuid
          project4Ctn.replies
          project4Ctn.labels
    , resolvedQuestionUuids = projectMigrationDto.resolvedQuestionUuids
    , tenantUuid = defaultTenant.uuid
    }

projectMigrationDtoEdited :: ProjectMigrationDTO
projectMigrationDtoEdited =
  ProjectMigrationDTO
    { oldProject = projectMigrationDto.oldProject
    , newProject = projectMigrationDto.newProject
    , resolvedQuestionUuids = [question2.uuid, question3.uuid]
    , tenantUuid = projectMigrationDto.tenantUuid
    }

projectMigrationCreateDto :: ProjectMigrationCreateDTO
projectMigrationCreateDto =
  ProjectMigrationCreateDTO
    { targetKnowledgeModelPackageId = netherlandsKmPackageV2.pId
    , targetTagUuids = project4Upgraded.selectedQuestionTagUuids
    }

projectMigrationChangeDto :: ProjectMigrationChangeDTO
projectMigrationChangeDto =
  ProjectMigrationChangeDTO
    { resolvedQuestionUuids = projectMigrationDtoEdited.resolvedQuestionUuids
    }

differentProjectMigration :: ProjectMigration
differentProjectMigration =
  ProjectMigration
    { oldProjectUuid = differentProject.uuid
    , newProjectUuid = differentProject.uuid
    , resolvedQuestionUuids = [question2.uuid]
    , tenantUuid = differentTenant.uuid
    }
