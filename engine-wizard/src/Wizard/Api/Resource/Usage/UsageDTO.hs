module Wizard.Api.Resource.Usage.UsageDTO where

import GHC.Generics
import GHC.Int

data UsageDTO = UsageDTO
  { users :: UsageEntryDTO
  , activeUsers :: UsageEntryDTO
  , knowledgeModels :: UsageEntryDTO
  , branches :: UsageEntryDTO
  , templates :: UsageEntryDTO
  , questionnaires :: UsageEntryDTO
  , documents :: UsageEntryDTO
  , storage :: UsageEntryDTO
  }
  deriving (Show, Eq, Generic)

data UsageEntryDTO = UsageEntryDTO
  { current :: Int64
  , max :: Maybe Int64
  }
  deriving (Show, Eq, Generic)
