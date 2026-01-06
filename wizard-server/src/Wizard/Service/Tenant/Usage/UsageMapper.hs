module Wizard.Service.Tenant.Usage.UsageMapper where

import GHC.Int

import Shared.Common.Api.Resource.Tenant.Usage.UsageEntryDTO
import Wizard.Model.Tenant.Limit.TenantLimitBundle
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO

toDTO :: TenantLimitBundle -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int64 -> WizardUsageDTO
toDTO limitBundle userCount activeUserCount knowledgeModelEditorCount kmCount prjCount tmlCount tmlDraftCount docCount localeCount storageSize =
  WizardUsageDTO
    { users =
        UsageEntryDTO
          { current = fromIntegral userCount
          , max = fromIntegral limitBundle.users
          }
    , activeUsers =
        UsageEntryDTO
          { current = fromIntegral activeUserCount
          , max = fromIntegral limitBundle.activeUsers
          }
    , knowledgeModels =
        UsageEntryDTO
          { current = fromIntegral kmCount
          , max = fromIntegral limitBundle.knowledgeModels
          }
    , knowledgeModelEditors =
        UsageEntryDTO
          { current = fromIntegral knowledgeModelEditorCount
          , max = fromIntegral limitBundle.knowledgeModelEditors
          }
    , documentTemplates =
        UsageEntryDTO
          { current = fromIntegral tmlCount
          , max = fromIntegral limitBundle.documentTemplates
          }
    , documentTemplateDrafts =
        UsageEntryDTO
          { current = fromIntegral tmlDraftCount
          , max = fromIntegral limitBundle.documentTemplateDrafts
          }
    , projects =
        UsageEntryDTO
          { current = fromIntegral prjCount
          , max = fromIntegral limitBundle.projects
          }
    , documents =
        UsageEntryDTO
          { current = fromIntegral docCount
          , max = fromIntegral limitBundle.documents
          }
    , locales =
        UsageEntryDTO
          { current = fromIntegral localeCount
          , max = fromIntegral limitBundle.locales
          }
    , storage = UsageEntryDTO {current = storageSize, max = limitBundle.storage}
    }
