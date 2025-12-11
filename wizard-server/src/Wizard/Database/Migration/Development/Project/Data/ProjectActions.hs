module Wizard.Database.Migration.Development.Project.Data.ProjectActions where

import qualified Data.Aeson.KeyMap as KM

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.Project.Action.ProjectActionDTO
import Wizard.Constant.ProjectAction
import Wizard.Model.Project.Action.ProjectAction
import Wizard.Service.Project.Action.ProjectActionMapper

projectActionFtp1 :: ProjectAction
projectActionFtp1 =
  ProjectAction
    { paId = "global:project-action-ftp:1.0.0"
    , name = "Project Action FTP"
    , organizationId = "global"
    , actionId = "project-action-ftp"
    , version = "1.0.0"
    , metamodelVersion = projectActionMetamodelVersion
    , description = "Uploading project to FTP"
    , readme = "# Project Action FTP"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , url = "http://example.com/project-action-ftp"
    , config = KM.empty
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

projectActionFtp2 :: ProjectAction
projectActionFtp2 =
  ProjectAction
    { paId = "global:project-action-ftp:2.0.0"
    , name = "Project Action FTP"
    , organizationId = "global"
    , actionId = "project-action-ftp"
    , version = "2.0.0"
    , metamodelVersion = projectActionMetamodelVersion
    , description = "Uploading project to FTP"
    , readme = "# Project Action FTP"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , url = "http://example.com/project-action-ftp"
    , config = KM.empty
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

projectActionFtp3 :: ProjectAction
projectActionFtp3 =
  ProjectAction
    { paId = "global:project-action-ftp:3.0.0"
    , name = "Project Action FTP"
    , organizationId = "global"
    , actionId = "project-action-ftp"
    , version = "3.0.0"
    , metamodelVersion = projectActionMetamodelVersion
    , description = "Uploading project to FTP"
    , readme = "# Project Action FTP"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , url = "http://example.com/project-action-ftp"
    , config = KM.empty
    , enabled = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

projectActionFtp3Edited :: ProjectAction
projectActionFtp3Edited = projectActionFtp3 {enabled = True}

projectActionFtp3Dto :: ProjectActionDTO
projectActionFtp3Dto = toDTO projectActionFtp3

projectActionMail1 :: ProjectAction
projectActionMail1 =
  ProjectAction
    { paId = "global:project-action-mail:1.0.0"
    , name = "Project Action Mail"
    , organizationId = "global"
    , actionId = "project-action-mail"
    , version = "1.0.0"
    , metamodelVersion = projectActionMetamodelVersion
    , description = "Sending project via mail"
    , readme = "# Project Action Mail"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternGlobal]
    , url = "http://example.com/project-action-mail"
    , config = KM.empty
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

projectActionScp1 :: ProjectAction
projectActionScp1 =
  ProjectAction
    { paId = "global:project-action-onto:1.0.0"
    , name = "Project Action SCP"
    , organizationId = "global"
    , actionId = "project-action-onto"
    , version = "1.0.0"
    , metamodelVersion = projectActionMetamodelVersion
    , description = "Uploading project via SCP"
    , readme = "# Project Action SCP"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternGlobal]
    , url = "http://example.com/project-action-onto"
    , config = KM.empty
    , enabled = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
