module WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantUsages where

import Shared.Common.Api.Resource.Tenant.Usage.UsageEntryDTO
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO

defaultUsage :: WizardUsageDTO
defaultUsage =
  WizardUsageDTO
    { users = defaultUsageUsers
    , activeUsers = defaultUsageActiveUsers
    , knowledgeModels = defaultUsageKnowledgeModels
    , branches = defaultUsageBranches
    , documentTemplates = defaultUsageDocumentTemplates
    , documentTemplateDrafts = defaultUsageDocumentTemplateDrafts
    , questionnaires = defaultUsageQuestionnaires
    , documents = defaultUsageDocuments
    , locales = defaultUsageLocales
    , storage = defaultUsageStorage
    }

defaultUsageEdited :: WizardUsageDTO
defaultUsageEdited =
  defaultUsage
    { users = defaultUsageUsers {max = -2000}
    }

defaultUsageUsers :: UsageEntryDTO
defaultUsageUsers = UsageEntryDTO {current = 1, max = -1000}

defaultUsageActiveUsers :: UsageEntryDTO
defaultUsageActiveUsers = UsageEntryDTO {current = 1, max = -1000}

defaultUsageKnowledgeModels :: UsageEntryDTO
defaultUsageKnowledgeModels = UsageEntryDTO {current = 2, max = -1000}

defaultUsageBranches :: UsageEntryDTO
defaultUsageBranches = UsageEntryDTO {current = 0, max = -1000}

defaultUsageDocumentTemplates :: UsageEntryDTO
defaultUsageDocumentTemplates = UsageEntryDTO {current = 0, max = -1000}

defaultUsageDocumentTemplateDrafts :: UsageEntryDTO
defaultUsageDocumentTemplateDrafts = UsageEntryDTO {current = 0, max = -1000}

defaultUsageQuestionnaires :: UsageEntryDTO
defaultUsageQuestionnaires = UsageEntryDTO {current = 0, max = -1000}

defaultUsageDocuments :: UsageEntryDTO
defaultUsageDocuments = UsageEntryDTO {current = 0, max = -1000}

defaultUsageLocales :: UsageEntryDTO
defaultUsageLocales = UsageEntryDTO {current = 0, max = -1000}

defaultUsageStorage :: UsageEntryDTO
defaultUsageStorage = UsageEntryDTO {current = 0, max = -1000 * 5 * 1000 * 1000}
