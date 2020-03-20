module Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.ActionKey.ActionKey

regActionKey =
  ActionKey
    { _actionKeyUuid = fromJust . U.fromString $ "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , _actionKeyUserId = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    , _actionKeyAType = RegistrationActionKey
    , _actionKeyHash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , _actionKeyCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgPassActionKey =
  ActionKey
    { _actionKeyUuid = fromJust . U.fromString $ "23f934f2-05b2-45d3-bce9-7675c3f3e5e9"
    , _actionKeyUserId = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    , _actionKeyAType = ForgottenPasswordActionKey
    , _actionKeyHash = "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    , _actionKeyCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

forgTokActionKeyDto =
  ActionKeyDTO {_actionKeyDTOAType = forgPassActionKey ^. aType, _actionKeyDTOEmail = userAlbert ^. email}
