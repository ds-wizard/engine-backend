module Wizard.Database.Migration.Development.Limit.Data.AppLimits where

import Shared.Constant.App
import Shared.Util.Date
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import Wizard.Model.Limit.AppLimit

defaultAppLimit :: AppLimit
defaultAppLimit =
  AppLimit
    { uuid = defaultAppUuid
    , users = Nothing
    , activeUsers = Nothing
    , knowledgeModels = Nothing
    , branches = Nothing
    , templates = Nothing
    , questionnaires = Nothing
    , documents = Nothing
    , storage = Nothing
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }

differentAppLimit :: AppLimit
differentAppLimit =
  AppLimit
    { uuid = differentApp.uuid
    , users = Nothing
    , activeUsers = Nothing
    , knowledgeModels = Nothing
    , branches = Nothing
    , templates = Nothing
    , questionnaires = Nothing
    , documents = Nothing
    , storage = Nothing
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }
