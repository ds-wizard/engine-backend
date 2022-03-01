module Wizard.Service.Usage.UsageMapper where

import Control.Lens ((^.))
import GHC.Int

import LensesConfig
import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Model.Limit.AppLimit

toDTO :: AppLimit -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int64 -> UsageDTO
toDTO appLimit userCount activeUserCount branchCount kmCount qtnCount tmlCount docCount storageSize =
  UsageDTO
    { _usageDTOUsers =
        UsageEntryDTO
          {_usageEntryDTOCurrent = fromIntegral userCount, _usageEntryDTOMax = fmap fromIntegral $ appLimit ^. users}
    , _usageDTOActiveUsers =
        UsageEntryDTO
          { _usageEntryDTOCurrent = fromIntegral activeUserCount
          , _usageEntryDTOMax = fmap fromIntegral $ appLimit ^. activeUsers
          }
    , _usageDTOKnowledgeModels =
        UsageEntryDTO
          { _usageEntryDTOCurrent = fromIntegral kmCount
          , _usageEntryDTOMax = fmap fromIntegral $ appLimit ^. knowledgeModels
          }
    , _usageDTOBranches =
        UsageEntryDTO
          { _usageEntryDTOCurrent = fromIntegral branchCount
          , _usageEntryDTOMax = fmap fromIntegral $ appLimit ^. branches
          }
    , _usageDTOTemplates =
        UsageEntryDTO
          {_usageEntryDTOCurrent = fromIntegral tmlCount, _usageEntryDTOMax = fmap fromIntegral $ appLimit ^. templates}
    , _usageDTOQuestionnaires =
        UsageEntryDTO
          { _usageEntryDTOCurrent = fromIntegral qtnCount
          , _usageEntryDTOMax = fmap fromIntegral $ appLimit ^. questionnaires
          }
    , _usageDTODocuments =
        UsageEntryDTO
          {_usageEntryDTOCurrent = fromIntegral docCount, _usageEntryDTOMax = fmap fromIntegral $ appLimit ^. documents}
    , _usageDTOStorage = UsageEntryDTO {_usageEntryDTOCurrent = storageSize, _usageEntryDTOMax = appLimit ^. storage}
    }
