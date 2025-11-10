module WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO where

import GHC.Generics

import Shared.Common.Api.Resource.Tenant.Usage.UsageEntryDTO

data WizardUsageDTO = WizardUsageDTO
  { users :: UsageEntryDTO
  , activeUsers :: UsageEntryDTO
  , knowledgeModels :: UsageEntryDTO
  , knowledgeModelEditors :: UsageEntryDTO
  , documentTemplates :: UsageEntryDTO
  , documentTemplateDrafts :: UsageEntryDTO
  , questionnaires :: UsageEntryDTO
  , documents :: UsageEntryDTO
  , locales :: UsageEntryDTO
  , storage :: UsageEntryDTO
  }
  deriving (Show, Eq, Generic)
