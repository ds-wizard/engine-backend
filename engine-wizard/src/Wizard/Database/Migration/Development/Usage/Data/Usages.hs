module Wizard.Database.Migration.Development.Usage.Data.Usages where

import Wizard.Api.Resource.Usage.UsageDTO

defaultUsage :: UsageDTO
defaultUsage =
  UsageDTO
    { _usageDTOUsers = defaultUsageUsers
    , _usageDTOActiveUsers = defaultUsageActiveUsers
    , _usageDTOKnowledgeModels = defaultUsageKnowledgeModels
    , _usageDTOBranches = defaultUsageBranches
    , _usageDTOTemplates = defaultUsageTemplates
    , _usageDTOQuestionnaires = defaultUsageQuestionnaires
    , _usageDTODocuments = defaultUsageDocuments
    , _usageDTOStorage = defaultUsageStorage
    }

defaultUsageUsers :: UsageEntryDTO
defaultUsageUsers = UsageEntryDTO {_usageEntryDTOCurrent = 3, _usageEntryDTOMax = Nothing}

defaultUsageActiveUsers :: UsageEntryDTO
defaultUsageActiveUsers = UsageEntryDTO {_usageEntryDTOCurrent = 3, _usageEntryDTOMax = Nothing}

defaultUsageKnowledgeModels :: UsageEntryDTO
defaultUsageKnowledgeModels = UsageEntryDTO {_usageEntryDTOCurrent = 3, _usageEntryDTOMax = Nothing}

defaultUsageBranches :: UsageEntryDTO
defaultUsageBranches = UsageEntryDTO {_usageEntryDTOCurrent = 3, _usageEntryDTOMax = Nothing}

defaultUsageTemplates :: UsageEntryDTO
defaultUsageTemplates = UsageEntryDTO {_usageEntryDTOCurrent = 3, _usageEntryDTOMax = Nothing}

defaultUsageQuestionnaires :: UsageEntryDTO
defaultUsageQuestionnaires = UsageEntryDTO {_usageEntryDTOCurrent = 3, _usageEntryDTOMax = Nothing}

defaultUsageDocuments :: UsageEntryDTO
defaultUsageDocuments = UsageEntryDTO {_usageEntryDTOCurrent = 3, _usageEntryDTOMax = Nothing}

defaultUsageStorage :: UsageEntryDTO
defaultUsageStorage = UsageEntryDTO {_usageEntryDTOCurrent = 3, _usageEntryDTOMax = Nothing}
