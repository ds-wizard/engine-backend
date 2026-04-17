module WizardLib.Public.Service.OpenId.Client.Definition.OpenIdClientDefinitionMapper where

import Data.Time
import qualified Data.UUID as U

import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import WizardLib.Public.Model.OpenId.OpenIdClient
import WizardLib.Public.Model.OpenId.OpenIdClientSimple

toSimple :: OpenIdClient -> OpenIdClientSimple
toSimple openIdClient =
  OpenIdClientSimple
    { uuid = openIdClient.uuid
    , name = openIdClient.name
    , url = openIdClient.url
    , style = openIdClient.style
    }

toDetailDTO :: OpenIdClient -> OpenIdClientDetailDTO
toDetailDTO openIdClient =
  OpenIdClientDetailDTO
    { uuid = openIdClient.uuid
    , name = openIdClient.name
    , url = openIdClient.url
    , clientId = openIdClient.clientId
    , clientSecret = openIdClient.clientSecret
    , parameters = openIdClient.parameters
    , style = openIdClient.style
    , registrationEnabled = openIdClient.registrationEnabled
    , scopeProfile = openIdClient.scopeProfile
    , scopeEmail = openIdClient.scopeEmail
    , tenantUuid = openIdClient.tenantUuid
    , createdAt = openIdClient.createdAt
    , updatedAt = openIdClient.updatedAt
    }

fromCreateDTO :: OpenIdClientChangeDTO -> U.UUID -> U.UUID -> UTCTime -> OpenIdClient
fromCreateDTO reqDto uuid tenantUuid now =
  OpenIdClient
    { uuid = uuid
    , name = reqDto.name
    , url = reqDto.url
    , clientId = reqDto.clientId
    , clientSecret = reqDto.clientSecret
    , parameters = reqDto.parameters
    , style = reqDto.style
    , registrationEnabled = reqDto.registrationEnabled
    , scopeProfile = reqDto.scopeProfile
    , scopeEmail = reqDto.scopeEmail
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: OpenIdClient -> OpenIdClientChangeDTO -> UTCTime -> OpenIdClient
fromChangeDTO openIdClient reqDto now =
  OpenIdClient
    { uuid = openIdClient.uuid
    , name = reqDto.name
    , url = reqDto.url
    , clientId = reqDto.clientId
    , clientSecret = reqDto.clientSecret
    , parameters = reqDto.parameters
    , style = reqDto.style
    , registrationEnabled = reqDto.registrationEnabled
    , scopeProfile = reqDto.scopeProfile
    , scopeEmail = reqDto.scopeEmail
    , tenantUuid = openIdClient.tenantUuid
    , createdAt = openIdClient.createdAt
    , updatedAt = now
    }
