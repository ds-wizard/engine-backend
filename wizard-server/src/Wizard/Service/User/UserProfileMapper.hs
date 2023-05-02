module Wizard.Service.User.UserProfileMapper where

import Data.Char (toLower)
import qualified Data.Map.Strict as M

import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.User.User

toUserProfileDTO :: User -> [UserSubmissionPropsDTO] -> UserProfileDTO
toUserProfileDTO user props =
  UserProfileDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , email = user.email
    , affiliation = user.affiliation
    , sources = user.sources
    , uRole = user.uRole
    , permissions = user.permissions
    , active = user.active
    , submissionProps = props
    , imageUrl = user.imageUrl
    , createdAt = user.createdAt
    , updatedAt = user.updatedAt
    }

toUserSubmissionPropsDTO :: UserSubmissionProps -> String -> UserSubmissionPropsDTO
toUserSubmissionPropsDTO props name =
  UserSubmissionPropsDTO
    { sId = props.sId
    , name = name
    , values = props.values
    }

fromUserProfileChangeDTO :: UserProfileChangeDTO -> User -> User
fromUserProfileChangeDTO dto oldUser =
  User
    { uuid = oldUser.uuid
    , firstName = dto.firstName
    , lastName = dto.lastName
    , email = toLower <$> dto.email
    , passwordHash = oldUser.passwordHash
    , affiliation = dto.affiliation
    , sources = oldUser.sources
    , uRole = oldUser.uRole
    , permissions = oldUser.permissions
    , active = oldUser.active
    , submissionProps = fromUserSubmissionPropsDTO <$> dto.submissionProps
    , imageUrl = oldUser.imageUrl
    , groups = oldUser.groups
    , machine = oldUser.machine
    , appUuid = oldUser.appUuid
    , lastVisitedAt = oldUser.lastVisitedAt
    , createdAt = oldUser.createdAt
    , updatedAt = oldUser.updatedAt
    }

fromUserSubmissionPropsDTO :: UserSubmissionPropsDTO -> UserSubmissionProps
fromUserSubmissionPropsDTO dto =
  UserSubmissionProps {sId = dto.sId, values = dto.values}

fromService :: AppConfigSubmissionService -> UserSubmissionPropsDTO
fromService service =
  UserSubmissionPropsDTO
    { sId = service.sId
    , name = service.name
    , values = M.fromList (fmap (\v -> (v, "")) service.props)
    }

fromUserSubmissionProps :: UserSubmissionProps -> UserSubmissionPropsDTO
fromUserSubmissionProps props =
  UserSubmissionPropsDTO
    { sId = props.sId
    , name = ""
    , values = props.values
    }
