module Wizard.Service.User.Profile.UserProfileMapper where

import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.UUID as U

import Data.Time (UTCTime)
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Model.User.User
import Wizard.Model.User.UserSubmissionProp
import Wizard.Model.User.UserSubmissionPropList

fromUserProfileChangeDTO :: UserProfileChangeDTO -> User -> Bool -> UTCTime -> User
fromUserProfileChangeDTO dto oldUser revertPending now =
  let newEmail = toLower <$> dto.email
      emailChanged = newEmail /= oldUser.email
      newEmailVerifiedAt
        | emailChanged = Nothing
        | revertPending = Just now
        | otherwise = oldUser.emailVerifiedAt
      newEmailPending
        | emailChanged = Just newEmail
        | revertPending = Nothing
        | otherwise = oldUser.emailPending
   in User
        { uuid = oldUser.uuid
        , firstName = dto.firstName
        , lastName = dto.lastName
        , email = oldUser.email
        , passwordHash = oldUser.passwordHash
        , affiliation = dto.affiliation
        , role = oldUser.role
        , active = oldUser.active
        , imageUrl = oldUser.imageUrl
        , locale = oldUser.locale
        , machine = oldUser.machine
        , lastSeenNewsId = oldUser.lastSeenNewsId
        , tenantUuid = oldUser.tenantUuid
        , lastVisitedAt = oldUser.lastVisitedAt
        , createdAt = oldUser.createdAt
        , updatedAt = now
        , emailVerifiedAt = newEmailVerifiedAt
        , emailPending = newEmailPending
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
