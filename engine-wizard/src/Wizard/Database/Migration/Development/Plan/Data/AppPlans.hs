module Wizard.Database.Migration.Development.Plan.Data.AppPlans where

import Shared.Constant.App
import Shared.Util.Date
import Shared.Util.Uuid
import Wizard.Model.Plan.AppPlan

standardPlan :: AppPlan
standardPlan =
  AppPlan
    { uuid = u' "f54f7b16-804b-439f-b8db-abbae171f18b"
    , name = "Standard"
    , users = Nothing
    , since = Just $ dt' 2022 1 25
    , until = Just $ dt' 2030 1 25
    , test = False
    , appUuid = defaultAppUuid
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }

standardPlanEdited :: AppPlan
standardPlanEdited =
  standardPlan
    { name = "EDITED: Standard"
    , users = Just 50
    , since = Just $ dt' 2022 2 25
    , until = Just $ dt' 2030 2 25
    , test = True
    }

standardPlanExpired :: AppPlan
standardPlanExpired =
  AppPlan
    { uuid = u' "02cdb3ae-e33c-4b84-a181-716f5cd76d08"
    , name = "Standard"
    , users = Nothing
    , since = Just $ dt' 2020 1 25
    , until = Just $ dt' 2022 1 25
    , test = False
    , appUuid = defaultAppUuid
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }
