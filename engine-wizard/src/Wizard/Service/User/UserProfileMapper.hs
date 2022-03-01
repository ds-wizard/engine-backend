module Wizard.Service.User.UserProfileMapper where

import Control.Lens ((^.))
import Data.Char (toLower)
import qualified Data.Map.Strict as M

import LensesConfig
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.User.User

toUserProfileDTO :: User -> [UserSubmissionPropsDTO] -> UserProfileDTO
toUserProfileDTO user props =
  UserProfileDTO
    { _userProfileDTOUuid = user ^. uuid
    , _userProfileDTOFirstName = user ^. firstName
    , _userProfileDTOLastName = user ^. lastName
    , _userProfileDTOEmail = user ^. email
    , _userProfileDTOAffiliation = user ^. affiliation
    , _userProfileDTOSources = user ^. sources
    , _userProfileDTORole = user ^. role
    , _userProfileDTOPermissions = user ^. permissions
    , _userProfileDTOActive = user ^. active
    , _userProfileDTOSubmissionProps = props
    , _userProfileDTOImageUrl = user ^. imageUrl
    , _userProfileDTOCreatedAt = user ^. createdAt
    , _userProfileDTOUpdatedAt = user ^. updatedAt
    }

toUserSubmissionPropsDTO :: UserSubmissionProps -> String -> UserSubmissionPropsDTO
toUserSubmissionPropsDTO props name =
  UserSubmissionPropsDTO
    { _userSubmissionPropsDTOSId = props ^. sId
    , _userSubmissionPropsDTOName = name
    , _userSubmissionPropsDTOValues = props ^. values
    }

fromUserProfileChangeDTO :: UserProfileChangeDTO -> User -> User
fromUserProfileChangeDTO dto oldUser =
  User
    { _userUuid = oldUser ^. uuid
    , _userFirstName = dto ^. firstName
    , _userLastName = dto ^. lastName
    , _userEmail = toLower <$> dto ^. email
    , _userPasswordHash = oldUser ^. passwordHash
    , _userAffiliation = dto ^. affiliation
    , _userSources = oldUser ^. sources
    , _userRole = oldUser ^. role
    , _userPermissions = oldUser ^. permissions
    , _userActive = oldUser ^. active
    , _userSubmissionProps = fromUserSubmissionPropsDTO <$> dto ^. submissionProps
    , _userImageUrl = oldUser ^. imageUrl
    , _userGroups = oldUser ^. groups
    , _userMachine = oldUser ^. machine
    , _userAppUuid = oldUser ^. appUuid
    , _userLastVisitedAt = oldUser ^. lastVisitedAt
    , _userCreatedAt = oldUser ^. createdAt
    , _userUpdatedAt = oldUser ^. updatedAt
    }

fromUserSubmissionPropsDTO :: UserSubmissionPropsDTO -> UserSubmissionProps
fromUserSubmissionPropsDTO dto =
  UserSubmissionProps {_userSubmissionPropsSId = dto ^. sId, _userSubmissionPropsValues = dto ^. values}

fromService :: AppConfigSubmissionService -> UserSubmissionPropsDTO
fromService service =
  UserSubmissionPropsDTO
    { _userSubmissionPropsDTOSId = service ^. sId
    , _userSubmissionPropsDTOName = service ^. name
    , _userSubmissionPropsDTOValues = M.fromList (fmap (\v -> (v, "")) (service ^. props))
    }

fromUserSubmissionProps :: UserSubmissionProps -> UserSubmissionPropsDTO
fromUserSubmissionProps props =
  UserSubmissionPropsDTO
    { _userSubmissionPropsDTOSId = props ^. sId
    , _userSubmissionPropsDTOName = ""
    , _userSubmissionPropsDTOValues = props ^. values
    }
