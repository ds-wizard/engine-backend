module Wizard.Database.Migration.Development.User.Data.NikolaTesla where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.User.User

userNikola :: User
userNikola =
  User
    { _userUuid = u' "30d48cf4-8c8a-496f-bafe-585bd238f798"
    , _userFirstName = "Nikola"
    , _userLastName = "Tesla"
    , _userEmail = "nikola.tesla@example.com"
    , _userAffiliation = Nothing
    , _userSources = [_USER_SOURCE_INTERNAL]
    , _userRole = _USER_ROLE_DATA_STEWARD
    , _userPermissions =
        [ "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_IMPORTER_PERM"
        , "QTN_TML_PERM"
        , "DMP_PERM"
        , "SUBM_PERM"
        , "TML_PERM"
        ]
    , _userActive = True
    , _userPasswordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _userSubmissionProps = []
    , _userImageUrl = Nothing
    , _userGroups = [memberBioGroup, memberPlantGroup]
    , _userMachine = False
    , _userAppUuid = defaultApp ^. uuid
    , _userLastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , _userCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    , _userUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    }
