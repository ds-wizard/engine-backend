module Wizard.Database.Migration.Development.Limit.Data.AppLimits where

import Shared.Common.Constant.App
import Shared.Common.Util.Date
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
    , documentTemplates = Nothing
    , documentTemplateDrafts = Nothing
    , questionnaires = Nothing
    , documents = Nothing
    , locales = Nothing
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
    , documentTemplates = Nothing
    , documentTemplateDrafts = Nothing
    , questionnaires = Nothing
    , documents = Nothing
    , locales = Nothing
    , storage = Nothing
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }
