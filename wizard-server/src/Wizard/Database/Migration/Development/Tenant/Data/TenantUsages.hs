module Wizard.Database.Migration.Development.Tenant.Data.TenantUsages where

import Wizard.Api.Resource.Tenant.Usage.TenantUsageDTO

defaultTenantUsage :: TenantUsageDTO
defaultTenantUsage =
  TenantUsageDTO
    { users = defaultTenantUsageUsers
    , activeUsers = defaultTenantUsageActiveUsers
    , knowledgeModels = defaultTenantUsageKnowledgeModels
    , branches = defaultTenantUsageBranches
    , documentTemplates = defaultTenantUsageDocumentTemplates
    , documentTemplateDrafts = defaultTenantUsageDocumentTemplateDrafts
    , questionnaires = defaultTenantUsageQuestionnaires
    , documents = defaultTenantUsageDocuments
    , locales = defaultTenantUsageLocales
    , storage = defaultTenantUsageStorage
    }

defaultTenantUsageUsers :: TenantUsageEntryDTO
defaultTenantUsageUsers = TenantUsageEntryDTO {current = 1, max = Nothing}

defaultTenantUsageActiveUsers :: TenantUsageEntryDTO
defaultTenantUsageActiveUsers = TenantUsageEntryDTO {current = 1, max = Nothing}

defaultTenantUsageKnowledgeModels :: TenantUsageEntryDTO
defaultTenantUsageKnowledgeModels = TenantUsageEntryDTO {current = 2, max = Nothing}

defaultTenantUsageBranches :: TenantUsageEntryDTO
defaultTenantUsageBranches = TenantUsageEntryDTO {current = 0, max = Nothing}

defaultTenantUsageDocumentTemplates :: TenantUsageEntryDTO
defaultTenantUsageDocumentTemplates = TenantUsageEntryDTO {current = 0, max = Nothing}

defaultTenantUsageDocumentTemplateDrafts :: TenantUsageEntryDTO
defaultTenantUsageDocumentTemplateDrafts = TenantUsageEntryDTO {current = 0, max = Nothing}

defaultTenantUsageQuestionnaires :: TenantUsageEntryDTO
defaultTenantUsageQuestionnaires = TenantUsageEntryDTO {current = 0, max = Nothing}

defaultTenantUsageDocuments :: TenantUsageEntryDTO
defaultTenantUsageDocuments = TenantUsageEntryDTO {current = 0, max = Nothing}

defaultTenantUsageLocales :: TenantUsageEntryDTO
defaultTenantUsageLocales = TenantUsageEntryDTO {current = 0, max = Nothing}

defaultTenantUsageStorage :: TenantUsageEntryDTO
defaultTenantUsageStorage = TenantUsageEntryDTO {current = 0, max = Nothing}
