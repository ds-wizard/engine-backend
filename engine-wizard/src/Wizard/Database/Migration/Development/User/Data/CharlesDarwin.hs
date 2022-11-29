module Wizard.Database.Migration.Development.User.Data.CharlesDarwin where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Util.Uuid
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import Wizard.Model.User.User

userCharles :: User
userCharles =
  User
    { uuid = u' "1693daf3-fc48-4a93-a1fb-385e6c9fe7ac"
    , firstName = "Charles"
    , lastName = "Darwin"
    , email = "charles.darwin@example.com"
    , affiliation = Nothing
    , sources = [_USER_SOURCE_INTERNAL]
    , uRole = _USER_ROLE_RESEARCHER
    , permissions = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM", "SUBM_PERM"]
    , active = True
    , passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , submissionProps = []
    , imageUrl = Nothing
    , groups = []
    , machine = False
    , appUuid = differentApp.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
