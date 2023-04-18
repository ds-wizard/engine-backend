module Wizard.Database.Migration.Development.User.Data.NikolaTesla where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import Wizard.Model.User.User

userNikola :: User
userNikola =
  User
    { uuid = u' "30d48cf4-8c8a-496f-bafe-585bd238f798"
    , firstName = "Nikola"
    , lastName = "Tesla"
    , email = "nikola.tesla@example.com"
    , affiliation = Nothing
    , sources = [_USER_SOURCE_INTERNAL]
    , uRole = _USER_ROLE_DATA_STEWARD
    , permissions =
        [ "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_IMPORTER_PERM"
        , "QTN_TML_PERM"
        , "DOC_TML_READ_PERM"
        , "SUBM_PERM"
        , "DOC_TML_WRITE_PERM"
        ]
    , active = True
    , passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , submissionProps = []
    , imageUrl = Nothing
    , groups = [memberBioGroup, memberPlantGroup]
    , machine = False
    , appUuid = defaultApp.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    }
