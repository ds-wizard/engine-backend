module Wizard.Database.Migration.Development.User.Data.GalileoGalilei where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.User.User

userGalileo :: User
userGalileo =
  User
    { _userUuid = u' "549e294b-3301-416a-959c-e704b55a2544"
    , _userFirstName = "Galileo"
    , _userLastName = "Galileo"
    , _userEmail = "galileo.galileo@example.com"
    , _userAffiliation = Nothing
    , _userSources = [_USER_SOURCE_INTERNAL]
    , _userRole = _USER_ROLE_RESEARCHER
    , _userPermissions = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM", "SUBM_PERM"]
    , _userActive = True
    , _userPasswordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _userSubmissionProps = []
    , _userImageUrl = Nothing
    , _userGroups = []
    , _userMachine = False
    , _userAppUuid = defaultApp ^. uuid
    , _userLastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _userCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , _userUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
