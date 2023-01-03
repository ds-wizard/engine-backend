module Wizard.Database.Migration.Development.Usage.Data.Usages where

import Wizard.Api.Resource.Usage.UsageDTO

defaultUsage :: UsageDTO
defaultUsage =
  UsageDTO
    { users = defaultUsageUsers
    , activeUsers = defaultUsageActiveUsers
    , knowledgeModels = defaultUsageKnowledgeModels
    , branches = defaultUsageBranches
    , templates = defaultUsageTemplates
    , questionnaires = defaultUsageQuestionnaires
    , documents = defaultUsageDocuments
    , storage = defaultUsageStorage
    }

defaultUsageUsers :: UsageEntryDTO
defaultUsageUsers = UsageEntryDTO {current = 1, max = Nothing}

defaultUsageActiveUsers :: UsageEntryDTO
defaultUsageActiveUsers = UsageEntryDTO {current = 1, max = Nothing}

defaultUsageKnowledgeModels :: UsageEntryDTO
defaultUsageKnowledgeModels = UsageEntryDTO {current = 2, max = Nothing}

defaultUsageBranches :: UsageEntryDTO
defaultUsageBranches = UsageEntryDTO {current = 0, max = Nothing}

defaultUsageTemplates :: UsageEntryDTO
defaultUsageTemplates = UsageEntryDTO {current = 0, max = Nothing}

defaultUsageQuestionnaires :: UsageEntryDTO
defaultUsageQuestionnaires = UsageEntryDTO {current = 0, max = Nothing}

defaultUsageDocuments :: UsageEntryDTO
defaultUsageDocuments = UsageEntryDTO {current = 0, max = Nothing}

defaultUsageStorage :: UsageEntryDTO
defaultUsageStorage = UsageEntryDTO {current = 0, max = Nothing}
