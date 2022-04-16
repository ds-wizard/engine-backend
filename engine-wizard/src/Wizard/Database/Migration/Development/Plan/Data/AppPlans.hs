module Wizard.Database.Migration.Development.Plan.Data.AppPlans where

import Shared.Constant.App
import Shared.Util.Date
import Shared.Util.Uuid
import Wizard.Model.Plan.AppPlan

standardPlan :: AppPlan
standardPlan =
  AppPlan
    { _appPlanUuid = u' "f54f7b16-804b-439f-b8db-abbae171f18b"
    , _appPlanName = "Standard"
    , _appPlanUsers = Nothing
    , _appPlanSince = Just $ dt' 2022 1 25
    , _appPlanUntil = Just $ dt' 2030 1 25
    , _appPlanTest = False
    , _appPlanAppUuid = defaultAppUuid
    , _appPlanCreatedAt = dt' 2018 1 25
    , _appPlanUpdatedAt = dt' 2018 1 25
    }

standardPlanEdited :: AppPlan
standardPlanEdited =
  standardPlan
    { _appPlanName = "EDITED: Standard"
    , _appPlanUsers = Just 50
    , _appPlanSince = Just $ dt' 2022 2 25
    , _appPlanUntil = Just $ dt' 2030 2 25
    , _appPlanTest = True
    }

standardPlanExpired :: AppPlan
standardPlanExpired =
  AppPlan
    { _appPlanUuid = u' "02cdb3ae-e33c-4b84-a181-716f5cd76d08"
    , _appPlanName = "Standard"
    , _appPlanUsers = Nothing
    , _appPlanSince = Just $ dt' 2020 1 25
    , _appPlanUntil = Just $ dt' 2022 1 25
    , _appPlanTest = False
    , _appPlanAppUuid = defaultAppUuid
    , _appPlanCreatedAt = dt' 2018 1 25
    , _appPlanUpdatedAt = dt' 2018 1 25
    }
