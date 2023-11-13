module Wizard.Service.Tenant.Limit.LimitMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Model.Tenant.Limit.TenantLimitBundle

fromCreate :: U.UUID -> UTCTime -> TenantLimitBundle
fromCreate aUuid now =
  TenantLimitBundle
    { uuid = aUuid
    , users = Nothing
    , activeUsers = Nothing
    , knowledgeModels = Nothing
    , branches = Nothing
    , documentTemplates = Nothing
    , documentTemplateDrafts = Nothing
    , questionnaires = Nothing
    , documents = Nothing
    , locales = Nothing
    , storage = Nothing
    , createdAt = now
    , updatedAt = now
    }

fromChange :: TenantLimitBundle -> Maybe Int -> TenantLimitBundle
fromChange limitBundle mUsers =
  TenantLimitBundle
    { uuid = limitBundle.uuid
    , users = mUsers
    , activeUsers = mUsers
    , knowledgeModels = mUsers
    , branches = mUsers
    , documentTemplates = mUsers
    , documentTemplateDrafts = mUsers
    , questionnaires = fmap (* 2) mUsers
    , documents = fmap (* 5) mUsers
    , locales = mUsers
    , storage = fmap (fromIntegral . (* 5000000)) mUsers
    , createdAt = limitBundle.createdAt
    , updatedAt = limitBundle.updatedAt
    }
