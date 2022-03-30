module Wizard.Service.Plan.AppPlanMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (until)

import LensesConfig
import Wizard.Api.Resource.Plan.AppPlanChangeDTO
import Wizard.Model.Plan.AppPlan

toChangeDTO :: AppPlan -> AppPlanChangeDTO
toChangeDTO plan =
  AppPlanChangeDTO
    { _appPlanChangeDTOName = plan ^. name
    , _appPlanChangeDTOUsers = plan ^. users
    , _appPlanChangeDTOSince = plan ^. since
    , _appPlanChangeDTOUntil = plan ^. until
    , _appPlanChangeDTOTest = plan ^. test
    }

fromChangeDTO :: AppPlanChangeDTO -> U.UUID -> U.UUID -> UTCTime -> UTCTime -> AppPlan
fromChangeDTO reqDto uuid appUuid createdAt updatedAt =
  AppPlan
    { _appPlanUuid = uuid
    , _appPlanName = reqDto ^. name
    , _appPlanUsers = reqDto ^. users
    , _appPlanSince = reqDto ^. since
    , _appPlanUntil = reqDto ^. until
    , _appPlanTest = reqDto ^. test
    , _appPlanAppUuid = appUuid
    , _appPlanCreatedAt = createdAt
    , _appPlanUpdatedAt = updatedAt
    }
