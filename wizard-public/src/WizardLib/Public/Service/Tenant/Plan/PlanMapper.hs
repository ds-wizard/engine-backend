module WizardLib.Public.Service.Tenant.Plan.PlanMapper where

import Data.Time
import qualified Data.UUID as U
import Prelude hiding (until)

import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeDTO
import WizardLib.Public.Model.PersistentCommand.Tenant.Plan.CreateOrUpdatePlanCommand
import WizardLib.Public.Model.Tenant.Plan.TenantPlan

toChangeDTO :: TenantPlan -> TenantPlanChangeDTO
toChangeDTO plan =
  TenantPlanChangeDTO
    { name = plan.name
    , users = plan.users
    , since = plan.since
    , until = plan.until
    , test = plan.test
    }

fromChangeDTO :: TenantPlanChangeDTO -> U.UUID -> U.UUID -> UTCTime -> UTCTime -> TenantPlan
fromChangeDTO reqDto uuid tenantUuid createdAt updatedAt =
  TenantPlan
    { uuid = uuid
    , name = reqDto.name
    , users = reqDto.users
    , since = reqDto.since
    , until = reqDto.until
    , test = reqDto.test
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    , updatedAt = updatedAt
    }

fromCommandToChangeDTO :: CreateOrUpdatePlanCommand -> TenantPlanChangeDTO
fromCommandToChangeDTO command =
  TenantPlanChangeDTO
    { name = command.name
    , users = command.users
    , since = command.since
    , until = command.until
    , test = command.test
    }
