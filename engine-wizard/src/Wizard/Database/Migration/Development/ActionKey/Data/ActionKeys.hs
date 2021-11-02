module Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.ActionKey.ActionKey

regActionKey =
  ActionKey
    { _actionKeyUuid = u' "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , _actionKeyUserId = userAlbert ^. uuid
    , _actionKeyAType = RegistrationActionKey
    , _actionKeyHash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , _actionKeyAppUuid = defaultApp ^. uuid
    , _actionKeyCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgPassActionKey =
  ActionKey
    { _actionKeyUuid = u' "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , _actionKeyUserId = userAlbert ^. uuid
    , _actionKeyAType = ForgottenPasswordActionKey
    , _actionKeyHash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , _actionKeyAppUuid = defaultApp ^. uuid
    , _actionKeyCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgTokActionKeyDto =
  ActionKeyDTO {_actionKeyDTOAType = forgPassActionKey ^. aType, _actionKeyDTOEmail = userAlbert ^. email}

differentActionKey =
  ActionKey
    { _actionKeyUuid = u' "61feb6c8-3be6-4095-b2e8-7e63dcfd1f31"
    , _actionKeyUserId = userCharles ^. uuid
    , _actionKeyAType = RegistrationActionKey
    , _actionKeyHash = "b2da34b1-35b2-408b-8127-b0ab3b8b04d9"
    , _actionKeyAppUuid = differentApp ^. uuid
    , _actionKeyCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
