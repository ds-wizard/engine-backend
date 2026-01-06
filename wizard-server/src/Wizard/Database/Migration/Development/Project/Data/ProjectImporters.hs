module Wizard.Database.Migration.Development.Project.Data.ProjectImporters where

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Constant.ProjectImporter
import Wizard.Model.Project.Importer.ProjectImporter
import Wizard.Service.Project.Importer.ProjectImporterMapper

projectImporterBio1 :: ProjectImporter
projectImporterBio1 =
  ProjectImporter
    { piId = "global:project-importer-bio:1.0.0"
    , name = "ProjectImporterBio"
    , organizationId = "global"
    , importerId = "project-importer-bio"
    , version = "1.0.0"
    , metamodelVersion = projectImporterMetamodelVersion
    , description = "Import bio answers from project"
    , readme = "# Default ProjectImporter BIO 1"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , url = "http://example.com/project-importer-bio"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

projectImporterBio2 :: ProjectImporter
projectImporterBio2 =
  ProjectImporter
    { piId = "global:project-importer-bio:2.0.0"
    , name = "ProjectImporterBio"
    , organizationId = "global"
    , importerId = "project-importer-bio"
    , version = "2.0.0"
    , metamodelVersion = projectImporterMetamodelVersion
    , description = "Import bio answers from project"
    , readme = "# Default ProjectImporter BIO 2"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , url = "http://example.com/project-importer-bio"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

projectImporterBio3 :: ProjectImporter
projectImporterBio3 =
  ProjectImporter
    { piId = "global:project-importer-bio:3.0.0"
    , name = "ProjectImporterBio"
    , organizationId = "global"
    , importerId = "project-importer-bio"
    , version = "3.0.0"
    , metamodelVersion = projectImporterMetamodelVersion
    , description = "Import bio answers from project"
    , readme = "# Default ProjectImporter BIO 3"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , url = "http://example.com/project-importer-bio"
    , enabled = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

projectImporterBio3Edited :: ProjectImporter
projectImporterBio3Edited = projectImporterBio3 {enabled = True}

projectImporterBio3Dto :: ProjectImporterDTO
projectImporterBio3Dto = toDTO projectImporterBio3

projectImporterExt1 :: ProjectImporter
projectImporterExt1 =
  ProjectImporter
    { piId = "global:project-ext-importer:1.0.0"
    , name = "ProjectImporterExt"
    , organizationId = "global"
    , importerId = "project-ext-importer"
    , version = "1.0.0"
    , metamodelVersion = projectImporterMetamodelVersion
    , description = "Import ext answers from project"
    , readme = "# Default Ext ProjectImporter"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternGlobal]
    , url = "http://example.com/project-ext-importer"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

projectImporterOnto1 :: ProjectImporter
projectImporterOnto1 =
  ProjectImporter
    { piId = "global:project-importer-onto:1.0.0"
    , name = "ProjectImporterOnto"
    , organizationId = "global"
    , importerId = "project-importer-onto"
    , version = "1.0.0"
    , metamodelVersion = projectImporterMetamodelVersion
    , description = "Import onto answers from project"
    , readme = "# Default Ext ProjectImporter"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternGlobal]
    , url = "http://example.com/project-importer-onto"
    , enabled = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
