module Wizard.Service.Limit.AppLimitMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Model.Limit.AppLimit

fromCreate :: U.UUID -> UTCTime -> AppLimit
fromCreate aUuid now =
  AppLimit
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

fromChange :: AppLimit -> Maybe Int -> AppLimit
fromChange appLimit mUsers =
  AppLimit
    { uuid = appLimit.uuid
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
    , createdAt = appLimit.createdAt
    , updatedAt = appLimit.updatedAt
    }
