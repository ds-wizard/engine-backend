module Wizard.Service.Usage.UsageMapper where

import GHC.Int

import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Model.Limit.AppLimit

toDTO :: AppLimit -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int64 -> UsageDTO
toDTO appLimit userCount activeUserCount branchCount kmCount qtnCount tmlCount docCount storageSize =
  UsageDTO
    { users =
        UsageEntryDTO
          { current = fromIntegral userCount
          , max = fmap fromIntegral $ appLimit.users
          }
    , activeUsers =
        UsageEntryDTO
          { current = fromIntegral activeUserCount
          , max = fmap fromIntegral $ appLimit.activeUsers
          }
    , knowledgeModels =
        UsageEntryDTO
          { current = fromIntegral kmCount
          , max = fmap fromIntegral $ appLimit.knowledgeModels
          }
    , branches =
        UsageEntryDTO
          { current = fromIntegral branchCount
          , max = fmap fromIntegral $ appLimit.branches
          }
    , templates =
        UsageEntryDTO
          { current = fromIntegral tmlCount
          , max = fmap fromIntegral $ appLimit.templates
          }
    , questionnaires =
        UsageEntryDTO
          { current = fromIntegral qtnCount
          , max = fmap fromIntegral $ appLimit.questionnaires
          }
    , documents =
        UsageEntryDTO
          { current = fromIntegral docCount
          , max = fmap fromIntegral $ appLimit.documents
          }
    , storage = UsageEntryDTO {current = storageSize, max = appLimit.storage}
    }
