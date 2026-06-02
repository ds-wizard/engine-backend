module WizardLib.Public.Service.User.UserOpenIdIdentityMapper where

import Data.Time
import qualified Data.UUID as U

import WizardLib.Public.Api.Resource.User.UserOpenIdIdentityDTO
import WizardLib.Public.Model.User.UserOpenIdIdentity
import WizardLib.Public.Model.User.UserOpenIdIdentityList
import WizardLib.Public.Model.User.UserRegistrationPending

toDTO :: UserOpenIdIdentityList -> UserOpenIdIdentityDTO
toDTO entity =
  UserOpenIdIdentityDTO
    { uuid = entity.uuid
    , externalId = entity.externalId
    , externalLabel = entity.externalLabel
    , providerUuid = entity.providerUuid
    , providerName = entity.providerName
    , providerStyle = entity.providerStyle
    , createdAt = entity.createdAt
    }

fromCreate :: U.UUID -> String -> Maybe String -> U.UUID -> U.UUID -> U.UUID -> UTCTime -> UserOpenIdIdentity
fromCreate uuid externalId mExternalLabel userUuid providerUuid tenantUuid now =
  UserOpenIdIdentity
    { uuid = uuid
    , externalId = externalId
    , externalLabel = mExternalLabel
    , userUuid = userUuid
    , providerUuid = providerUuid
    , tenantUuid = tenantUuid
    , createdAt = now
    }

fromPending :: U.UUID -> UserRegistrationPending serviceType -> U.UUID -> UTCTime -> UserOpenIdIdentity
fromPending uuid pending userUuid now =
  UserOpenIdIdentity
    { uuid = uuid
    , externalId = pending.externalId
    , externalLabel = pending.externalLabel
    , userUuid = userUuid
    , providerUuid = pending.providerUuid
    , tenantUuid = pending.tenantUuid
    , createdAt = now
    }
