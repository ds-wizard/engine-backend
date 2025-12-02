module Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters where

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Constant.QuestionnaireImporter
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterMapper

questionnaireImporterBio1 :: QuestionnaireImporter
questionnaireImporterBio1 =
  QuestionnaireImporter
    { qiId = "global:questionnaire-importer-bio:1.0.0"
    , name = "QuestionnaireImporterBio"
    , organizationId = "global"
    , importerId = "questionnaire-importer-bio"
    , version = "1.0.0"
    , metamodelVersion = questionnaireImporterMetamodelVersion
    , description = "Import bio answers from questionnaire"
    , readme = "# Default QuestionnaireImporter BIO 1"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , url = "http://example.com/questionnaire-importer-bio"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

questionnaireImporterBio2 :: QuestionnaireImporter
questionnaireImporterBio2 =
  QuestionnaireImporter
    { qiId = "global:questionnaire-importer-bio:2.0.0"
    , name = "QuestionnaireImporterBio"
    , organizationId = "global"
    , importerId = "questionnaire-importer-bio"
    , version = "2.0.0"
    , metamodelVersion = questionnaireImporterMetamodelVersion
    , description = "Import bio answers from questionnaire"
    , readme = "# Default QuestionnaireImporter BIO 2"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , url = "http://example.com/questionnaire-importer-bio"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

questionnaireImporterBio3 :: QuestionnaireImporter
questionnaireImporterBio3 =
  QuestionnaireImporter
    { qiId = "global:questionnaire-importer-bio:3.0.0"
    , name = "QuestionnaireImporterBio"
    , organizationId = "global"
    , importerId = "questionnaire-importer-bio"
    , version = "3.0.0"
    , metamodelVersion = questionnaireImporterMetamodelVersion
    , description = "Import bio answers from questionnaire"
    , readme = "# Default QuestionnaireImporter BIO 3"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , url = "http://example.com/questionnaire-importer-bio"
    , enabled = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

questionnaireImporterBio3Edited :: QuestionnaireImporter
questionnaireImporterBio3Edited = questionnaireImporterBio3 {enabled = True}

questionnaireImporterBio3Dto :: QuestionnaireImporterDTO
questionnaireImporterBio3Dto = toDTO questionnaireImporterBio3

questionnaireImporterExt1 :: QuestionnaireImporter
questionnaireImporterExt1 =
  QuestionnaireImporter
    { qiId = "global:questionnaire-ext-importer:1.0.0"
    , name = "QuestionnaireImporterExt"
    , organizationId = "global"
    , importerId = "questionnaire-ext-importer"
    , version = "1.0.0"
    , metamodelVersion = questionnaireImporterMetamodelVersion
    , description = "Import ext answers from questionnaire"
    , readme = "# Default Ext QuestionnaireImporter"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternGlobal]
    , url = "http://example.com/questionnaire-ext-importer"
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

questionnaireImporterOnto1 :: QuestionnaireImporter
questionnaireImporterOnto1 =
  QuestionnaireImporter
    { qiId = "global:questionnaire-importer-onto:1.0.0"
    , name = "QuestionnaireImporterOnto"
    , organizationId = "global"
    , importerId = "questionnaire-importer-onto"
    , version = "1.0.0"
    , metamodelVersion = questionnaireImporterMetamodelVersion
    , description = "Import onto answers from questionnaire"
    , readme = "# Default Ext QuestionnaireImporter"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternGlobal]
    , url = "http://example.com/questionnaire-importer-onto"
    , enabled = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
