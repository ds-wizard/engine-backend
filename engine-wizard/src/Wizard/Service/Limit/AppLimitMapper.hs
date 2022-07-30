module Wizard.Service.Limit.AppLimitMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Model.Limit.AppLimit

fromCreate :: U.UUID -> UTCTime -> AppLimit
fromCreate aUuid now =
  AppLimit
    { _appLimitUuid = aUuid
    , _appLimitUsers = Nothing
    , _appLimitActiveUsers = Nothing
    , _appLimitKnowledgeModels = Nothing
    , _appLimitBranches = Nothing
    , _appLimitTemplates = Nothing
    , _appLimitQuestionnaires = Nothing
    , _appLimitDocuments = Nothing
    , _appLimitStorage = Nothing
    , _appLimitCreatedAt = now
    , _appLimitUpdatedAt = now
    }

fromChange :: AppLimit -> Maybe Int -> AppLimit
fromChange appLimit mUsers =
  AppLimit
    { _appLimitUuid = appLimit ^. uuid
    , _appLimitUsers = mUsers
    , _appLimitActiveUsers = mUsers
    , _appLimitKnowledgeModels = mUsers
    , _appLimitBranches = mUsers
    , _appLimitTemplates = mUsers
    , _appLimitQuestionnaires = fmap (* 2) mUsers
    , _appLimitDocuments = fmap (* 5) mUsers
    , _appLimitStorage = fmap (fromIntegral . (* 5000000)) mUsers
    , _appLimitCreatedAt = appLimit ^. createdAt
    , _appLimitUpdatedAt = appLimit ^. updatedAt
    }
