module Wizard.Service.User.Profile.UserProfileMapper where

import Data.Char (toLower)

import Data.Time (UTCTime)
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
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
    , machine = oldUser.machine
    , tenantUuid = oldUser.tenantUuid
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
