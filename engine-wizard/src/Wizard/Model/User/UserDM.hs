module Wizard.Model.User.UserDM where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U
import Wizard.Model.User.User

defaultUser :: User
defaultUser =
  User
    { _userUuid = U.nil
    , _userFirstName = "Albert"
    , _userLastName = "Einstein"
    , _userEmail = "albert.einstein@example.com"
    , _userAffiliation = Nothing
    , _userSources = [_USER_SOURCE_INTERNAL]
    , _userRole = _USER_ROLE_ADMIN
    , _userPermissions =
        [ "ADMIN_PERM"
        , "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_TML_PERM"
        , "DMP_PERM"
        , "CFG_PERM"
        , "SUBM_PERM"
        , "TML_PERM"
        , "DOC_PERM"
        ]
    , _userActive = True
    , _userPasswordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _userSubmissionProps = []
    , _userImageUrl = Nothing
    , _userGroups = []
    , _userAppUuid = U.nil
    , _userLastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _userCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _userUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }
