module Wizard.Api.Resource.Usage.UsageDTO where

import GHC.Generics
import GHC.Int

data UsageDTO =
  UsageDTO
    { _usageDTOUsers :: UsageEntryDTO
    , _usageDTOActiveUsers :: UsageEntryDTO
    , _usageDTOKnowledgeModels :: UsageEntryDTO
    , _usageDTOBranches :: UsageEntryDTO
    , _usageDTOTemplates :: UsageEntryDTO
    , _usageDTOQuestionnaires :: UsageEntryDTO
    , _usageDTODocuments :: UsageEntryDTO
    , _usageDTOStorage :: UsageEntryDTO
    }
  deriving (Show, Eq, Generic)

data UsageEntryDTO =
  UsageEntryDTO
    { _usageEntryDTOCurrent :: Int64
    , _usageEntryDTOMax :: Maybe Int64
    }
  deriving (Show, Eq, Generic)
