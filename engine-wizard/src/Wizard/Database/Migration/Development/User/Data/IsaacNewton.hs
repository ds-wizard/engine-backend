module Wizard.Database.Migration.Development.User.Data.IsaacNewton where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Database.Migration.Development.User.Data.AlbertEinstein
import Wizard.Model.User.User
import Wizard.Service.User.UserProfileMapper

userIsaac :: User
userIsaac =
  User
    { _userUuid = u' "e1c58e52-0824-4526-8ebe-ec38eec67030"
    , _userFirstName = "Isaac"
    , _userLastName = "Newton"
    , _userEmail = "isaac.newton@example.com"
    , _userAffiliation = Nothing
    , _userSources = [_USER_SOURCE_INTERNAL]
    , _userRole = _USER_ROLE_RESEARCHER
    , _userPermissions = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM", "SUBM_PERM"]
    , _userActive = True
    , _userPasswordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , _userSubmissionProps = []
    , _userImageUrl = Nothing
    , _userGroups = []
    , _userCreatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _userUpdatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

userIsaacEdited :: User
userIsaacEdited =
  userAlbert
    { _userFirstName = "EDITED: Isaac"
    , _userLastName = "EDITED: Newton"
    , _userEmail = "albert.einstein@example.com"
    , _userAffiliation = Just "EDITED: My University"
    , _userRole = _USER_ROLE_ADMIN
    , _userActive = True
    }

userIsaacEditedChange :: UserChangeDTO
userIsaacEditedChange =
  UserChangeDTO
    { _userChangeDTOFirstName = userIsaacEdited ^. firstName
    , _userChangeDTOLastName = userIsaacEdited ^. lastName
    , _userChangeDTOEmail = userIsaacEdited ^. email
    , _userChangeDTOAffiliation = userIsaacEdited ^. affiliation
    , _userChangeDTORole = userIsaacEdited ^. role
    , _userChangeDTOActive = userIsaacEdited ^. active
    }

userIsaacProfileChange :: UserProfileChangeDTO
userIsaacProfileChange =
  UserProfileChangeDTO
    { _userProfileChangeDTOFirstName = userAlbertEdited ^. firstName
    , _userProfileChangeDTOLastName = userAlbertEdited ^. lastName
    , _userProfileChangeDTOEmail = userAlbertEdited ^. email
    , _userProfileChangeDTOAffiliation = userAlbertEdited ^. affiliation
    , _userProfileChangeDTOSubmissionProps =
        [toUserSubmissionPropsDTO userAlbertApiTokenEdited (defaultSubmissionService ^. name)]
    }
