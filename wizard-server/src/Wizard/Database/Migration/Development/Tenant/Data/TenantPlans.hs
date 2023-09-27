module Wizard.Database.Migration.Development.Tenant.Data.TenantPlans where

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Model.Tenant.Plan.TenantPlan

standardPlan :: TenantPlan
standardPlan =
  TenantPlan
    { uuid = u' "f54f7b16-804b-439f-b8db-abbae171f18b"
    , name = "Standard"
    , users = Nothing
    , since = Just $ dt' 2022 1 25
    , until = Just $ dt' 2030 1 25
    , test = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }

standardPlanEdited :: TenantPlan
standardPlanEdited =
  standardPlan
    { name = "EDITED: Standard"
    , users = Just 50
    , since = Just $ dt' 2022 2 25
    , until = Just $ dt' 2030 2 25
    , test = True
    }

standardPlanExpired :: TenantPlan
standardPlanExpired =
  TenantPlan
    { uuid = u' "02cdb3ae-e33c-4b84-a181-716f5cd76d08"
    , name = "Standard"
    , users = Nothing
    , since = Just $ dt' 2020 1 25
    , until = Just $ dt' 2022 1 25
    , test = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }
