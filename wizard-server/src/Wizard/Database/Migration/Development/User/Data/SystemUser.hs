module Wizard.Database.Migration.Development.User.Data.SystemUser where

import Data.Maybe (fromJust)
import Data.Time

import Wizard.Constant.User
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import Wizard.Model.User.User

userSystem :: User
userSystem =
  User
    { uuid = systemUserUuid
    , firstName = "System"
    , lastName = "User"
    , email = "system@example.com"
    , affiliation = Nothing
    , sources = [_USER_SOURCE_INTERNAL]
    , uRole = _USER_ROLE_ADMIN
    , permissions =
        [ "APP_PERM"
        , "DEV_PERM"
        , "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_IMPORTER_PERM"
        , "QTN_TML_PERM"
        , "DOC_TML_READ_PERM"
        , "CFG_PERM"
        , "SUBM_PERM"
        , "DOC_TML_WRITE_PERM"
        , "DOC_PERM"
        , "LOC_PERM"
        ]
    , active = True
    , passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , submissionProps = []
    , imageUrl = Nothing
    , groups = []
    , machine = True
    , appUuid = defaultApp.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }
