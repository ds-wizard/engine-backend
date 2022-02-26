module Wizard.Service.Limit.AppLimitMapper where

import Data.Time
import qualified Data.UUID as U

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
