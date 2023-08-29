module Wizard.Service.User.UserProfileMapper where

import Data.Char (toLower)
import qualified Data.Map.Strict as M

import Data.Time (UTCTime)
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.User.User

toUserSubmissionPropsDTO :: UserSubmissionProps -> String -> UserSubmissionPropsDTO
toUserSubmissionPropsDTO props name =
  UserSubmissionPropsDTO
    { sId = props.sId
    , name = name
    , values = props.values
    }

fromUserProfileChangeDTO :: User -> UserProfileChangeDTO -> UTCTime -> User
fromUserProfileChangeDTO oldUser dto now =
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
    , submissionProps = oldUser.submissionProps
    , imageUrl = oldUser.imageUrl
    , groups = oldUser.groups
    , machine = oldUser.machine
    , appUuid = oldUser.appUuid
    , lastVisitedAt = oldUser.lastVisitedAt
    , createdAt = oldUser.createdAt
    , updatedAt = now
    }

fromUserSubmissionPropsDTO :: User -> [UserSubmissionPropsDTO] -> UTCTime -> User
fromUserSubmissionPropsDTO user submissionProps now =
  user
    { submissionProps = fmap (\sp -> UserSubmissionProps {sId = sp.sId, values = sp.values}) submissionProps
    , updatedAt = now
    }

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
