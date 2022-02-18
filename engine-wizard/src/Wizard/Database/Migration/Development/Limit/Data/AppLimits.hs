module Wizard.Database.Migration.Development.Limit.Data.AppLimits where

import Control.Lens ((^.))

import LensesConfig
import Shared.Constant.App
import Shared.Util.Date
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.Limit.AppLimit

defaultAppLimit :: AppLimit
defaultAppLimit =
  AppLimit
    { _appLimitUuid = defaultAppUuid
    , _appLimitUsers = Nothing
    , _appLimitActiveUsers = Nothing
    , _appLimitKnowledgeModels = Nothing
    , _appLimitBranches = Nothing
    , _appLimitTemplates = Nothing
    , _appLimitQuestionnaires = Nothing
    , _appLimitDocuments = Nothing
    , _appLimitStorage = Nothing
    , _appLimitCreatedAt = dt' 2018 1 25
    , _appLimitUpdatedAt = dt' 2018 1 25
    }

differentAppLimit :: AppLimit
differentAppLimit =
  AppLimit
    { _appLimitUuid = differentApp ^. uuid
    , _appLimitUsers = Nothing
    , _appLimitActiveUsers = Nothing
    , _appLimitKnowledgeModels = Nothing
    , _appLimitBranches = Nothing
    , _appLimitTemplates = Nothing
    , _appLimitQuestionnaires = Nothing
    , _appLimitDocuments = Nothing
    , _appLimitStorage = Nothing
    , _appLimitCreatedAt = dt' 2018 1 25
    , _appLimitUpdatedAt = dt' 2018 1 25
    }
