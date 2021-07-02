module Wizard.Database.Migration.Development.User.Data.NicolausCopernicus where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Util.Uuid
import Wizard.Model.User.User

userNicolaus :: User
userNicolaus =
  User
    { _userUuid = u' "d5035272-d0ac-4308-b4cc-3b78f7a61627"
    , _userFirstName = "Nicolaus"
    , _userLastName = "Copernicus"
    , _userEmail = "nicolaus.copernicus@example.com"
    , _userAffiliation = Nothing
    , _userSources = [_USER_SOURCE_INTERNAL]
    , _userRole = _USER_ROLE_RESEARCHER
    , _userPermissions = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM", "SUBM_PERM"]
    , _userActive = True
    , _userPasswordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _userSubmissionProps = []
    , _userImageUrl = Nothing
    , _userGroups = []
    , _userLastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _userCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _userUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }
