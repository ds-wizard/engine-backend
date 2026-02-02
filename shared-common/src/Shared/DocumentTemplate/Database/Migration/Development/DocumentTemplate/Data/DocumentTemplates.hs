module Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Uuid
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.DocumentTemplate.Constant.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages

wizardDocumentTemplate :: DocumentTemplate
wizardDocumentTemplate =
  DocumentTemplate
    { uuid = u' "557e76d8-338a-4664-886a-f6af2228776c"
    , name = "Project Report"
    , organizationId = "global"
    , templateId = "project-report"
    , version = "1.0.0"
    , phase = ReleasedDocumentTemplatePhase
    , metamodelVersion = documentTemplateMetamodelVersion
    , description = "Exported questions and answers from a project"
    , readme = "# Default DocumentTemplate"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , nonEditable = False
    , tenantUuid = defaultTenantUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

wizardDocumentTemplateCoordinate :: Coordinate
wizardDocumentTemplateCoordinate = Coordinate wizardDocumentTemplate.organizationId wizardDocumentTemplate.templateId wizardDocumentTemplate.version

wizardDocumentTemplateSimple :: DocumentTemplateSimple
wizardDocumentTemplateSimple = toSimple wizardDocumentTemplate

wizardDocumentTemplateDeprecated :: DocumentTemplate
wizardDocumentTemplateDeprecated = wizardDocumentTemplate {phase = DeprecatedDocumentTemplatePhase}

wizardDocumentTemplateDraft :: DocumentTemplate
wizardDocumentTemplateDraft =
  wizardDocumentTemplate
    { uuid = u' "d66e9224-caf5-45d4-a752-bf9dbae2d756"
    , name = "DRAFT: " ++ wizardDocumentTemplate.name
    , version = "2.0.0"
    , phase = DraftDocumentTemplatePhase
    , description = "DRAFT: " ++ wizardDocumentTemplate.description
    , allowedPackages = [kmPackagePatternAllEdited]
    }

wizardDocumentTemplateNlDraft :: DocumentTemplate
wizardDocumentTemplateNlDraft =
  wizardDocumentTemplate
    { uuid = u' "2d511419-deb3-476d-a470-02a039511500"
    , name = "New Document Template"
    , organizationId = "org.nl.amsterdam"
    , version = "3.0.0"
    , phase = DraftDocumentTemplatePhase
    , description = ""
    , allowedPackages = []
    }

anotherWizardDocumentTemplate :: DocumentTemplate
anotherWizardDocumentTemplate =
  DocumentTemplate
    { uuid = u' "1a6f6ca4-80b3-41b1-8821-518f0d12fa95"
    , name = "Another DocumentTemplate"
    , organizationId = "dsw"
    , templateId = "another-template"
    , version = "1.0.0"
    , phase = ReleasedDocumentTemplatePhase
    , metamodelVersion = documentTemplateMetamodelVersion
    , description = "This is a another template"
    , readme = "# Another DocumentTemplate"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , nonEditable = False
    , tenantUuid = defaultTenantUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

-- ---------------------------------------------------------------------------------------------------------------------
differentDocumentTemplate :: DocumentTemplate
differentDocumentTemplate =
  DocumentTemplate
    { uuid = u' "1026d13d-53ee-45c8-b322-cabc29e68f14"
    , name = "Another DocumentTemplate"
    , organizationId = "dsw"
    , templateId = "another-template"
    , version = "1.0.0"
    , phase = ReleasedDocumentTemplatePhase
    , metamodelVersion = documentTemplateMetamodelVersion
    , description = "This is a another template"
    , readme = "# Another DocumentTemplate"
    , license = "Apache-2.0"
    , allowedPackages = [kmPackagePatternAll]
    , nonEditable = False
    , tenantUuid = differentTenantUuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
