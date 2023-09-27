module Wizard.Service.Tenant.Usage.UsageMapper where

import GHC.Int

import Wizard.Api.Resource.Tenant.Usage.TenantUsageDTO
import Wizard.Model.Tenant.Limit.TenantLimitBundle

toDTO :: TenantLimitBundle -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int64 -> TenantUsageDTO
toDTO limitBundle userCount activeUserCount branchCount kmCount qtnCount tmlCount tmlDraftCount docCount localeCount storageSize =
  TenantUsageDTO
    { users =
        TenantUsageEntryDTO
          { current = fromIntegral userCount
          , max = fmap fromIntegral limitBundle.users
          }
    , activeUsers =
        TenantUsageEntryDTO
          { current = fromIntegral activeUserCount
          , max = fmap fromIntegral limitBundle.activeUsers
          }
    , knowledgeModels =
        TenantUsageEntryDTO
          { current = fromIntegral kmCount
          , max = fmap fromIntegral limitBundle.knowledgeModels
          }
    , branches =
        TenantUsageEntryDTO
          { current = fromIntegral branchCount
          , max = fmap fromIntegral limitBundle.branches
          }
    , documentTemplates =
        TenantUsageEntryDTO
          { current = fromIntegral tmlCount
          , max = fmap fromIntegral limitBundle.documentTemplates
          }
    , documentTemplateDrafts =
        TenantUsageEntryDTO
          { current = fromIntegral tmlDraftCount
          , max = fmap fromIntegral limitBundle.documentTemplateDrafts
          }
    , questionnaires =
        TenantUsageEntryDTO
          { current = fromIntegral qtnCount
          , max = fmap fromIntegral limitBundle.questionnaires
          }
    , documents =
        TenantUsageEntryDTO
          { current = fromIntegral docCount
          , max = fmap fromIntegral limitBundle.documents
          }
    , locales =
        TenantUsageEntryDTO
          { current = fromIntegral localeCount
          , max = fmap fromIntegral limitBundle.locales
          }
    , storage = TenantUsageEntryDTO {current = storageSize, max = limitBundle.storage}
    }
