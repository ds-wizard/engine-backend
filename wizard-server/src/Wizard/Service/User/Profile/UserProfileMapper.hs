module Wizard.Service.User.Profile.UserProfileMapper where

import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.UUID as U

import Data.Time (UTCTime)
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Model.User.User
import Wizard.Model.User.UserSubmissionProp
import Wizard.Model.User.UserSubmissionPropList

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
    , imageUrl = oldUser.imageUrl
    , locale = oldUser.locale
    , machine = oldUser.machine
    , tenantUuid = oldUser.tenantUuid
    , lastVisitedAt = oldUser.lastVisitedAt
    , createdAt = oldUser.createdAt
    , updatedAt = now
    }

fromUserSubmissionPropsDTO :: U.UUID -> U.UUID -> [UserSubmissionProp] -> [UserSubmissionPropList] -> UTCTime -> [UserSubmissionProp]
fromUserSubmissionPropsDTO userUuid tenantUuid submissionProps reqDtos now =
  let mapFn reqDto =
        UserSubmissionProp
          { userUuid = userUuid
          , serviceId = reqDto.sId
          , values = reqDto.values
          , tenantUuid = tenantUuid
          , createdAt =
              case L.find (\p -> p.serviceId == reqDto.sId) submissionProps of
                Just submissionProp -> submissionProp.createdAt
                Nothing -> now
          , updatedAt = now
          }
   in map mapFn reqDtos
