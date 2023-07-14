module Wizard.Service.Plan.AppPlanMapper where

import Data.Time
import qualified Data.UUID as U
import Prelude hiding (until)

import Wizard.Api.Resource.Plan.AppPlanChangeDTO
import Wizard.Model.Plan.AppPlan
import WizardLib.Public.Model.PersistentCommand.Plan.CreateOrUpdatePlanCommand

toChangeDTO :: AppPlan -> AppPlanChangeDTO
toChangeDTO plan =
  AppPlanChangeDTO
    { name = plan.name
    , users = plan.users
    , since = plan.since
    , until = plan.until
    , test = plan.test
    }

fromChangeDTO :: AppPlanChangeDTO -> U.UUID -> U.UUID -> UTCTime -> UTCTime -> AppPlan
fromChangeDTO reqDto uuid appUuid createdAt updatedAt =
  AppPlan
    { uuid = uuid
    , name = reqDto.name
    , users = reqDto.users
    , since = reqDto.since
    , until = reqDto.until
    , test = reqDto.test
    , appUuid = appUuid
    , createdAt = createdAt
    , updatedAt = updatedAt
    }

fromCommandToChangeDTO :: CreateOrUpdatePlanCommand -> AppPlanChangeDTO
fromCommandToChangeDTO command =
  AppPlanChangeDTO
    { name = command.name
    , users = command.users
    , since = command.since
    , until = command.until
    , test = command.test
    }
