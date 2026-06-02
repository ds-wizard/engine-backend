module WizardLib.Public.Database.Migration.Development.OpenId.Data.OpenIdClients where

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.OpenId.Database.Migration.Development.OpenId.Data.OpenIds
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import WizardLib.Public.Model.OpenId.OpenIdClient
import WizardLib.Public.Model.OpenId.OpenIdClientSimple

defaultOpenIdClient :: OpenIdClient
defaultOpenIdClient =
  OpenIdClient
    { uuid = u' "cb7558d8-5e78-4494-9b94-0e9d64676923"
    , name = "Google"
    , url = "https://accounts.google.com"
    , clientId = "32559869123-a98908094.apps.googleusercontent.com"
    , clientSecret = "sad89089023"
    , parameters = [openIdClientDefinitionParameter]
    , style = openIdClientDefinitionStyle
    , registrationEnabled = True
    , scopeProfile = True
    , scopeEmail = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 20
    }

defaultOpenIdClientSimple :: OpenIdClientSimple
defaultOpenIdClientSimple =
  OpenIdClientSimple
    { uuid = defaultOpenIdClient.uuid
    , name = defaultOpenIdClient.name
    , url = defaultOpenIdClient.url
    , style = defaultOpenIdClient.style
    }

defaultOpenIdClientDetailDto :: OpenIdClientDetailDTO
defaultOpenIdClientDetailDto =
  OpenIdClientDetailDTO
    { uuid = defaultOpenIdClient.uuid
    , name = defaultOpenIdClient.name
    , url = defaultOpenIdClient.url
    , clientId = defaultOpenIdClient.clientId
    , clientSecret = defaultOpenIdClient.clientSecret
    , parameters = defaultOpenIdClient.parameters
    , style = defaultOpenIdClient.style
    , registrationEnabled = defaultOpenIdClient.registrationEnabled
    , scopeProfile = defaultOpenIdClient.scopeProfile
    , scopeEmail = defaultOpenIdClient.scopeEmail
    , tenantUuid = defaultOpenIdClient.tenantUuid
    , createdAt = defaultOpenIdClient.createdAt
    , updatedAt = defaultOpenIdClient.updatedAt
    }

defaultOpenIdClientChangeDto :: OpenIdClientChangeDTO
defaultOpenIdClientChangeDto =
  OpenIdClientChangeDTO
    { name = "UPDATED: Google"
    , url = "https://accounts.google.com/updated"
    , clientId = "32559869123-a98908094.apps.googleusercontent.com/updated"
    , clientSecret = "sad89089023Updated"
    , parameters = [openIdClientDefinitionParameter]
    , style = openIdClientDefinitionStyle
    , registrationEnabled = True
    , scopeProfile = True
    , scopeEmail = True
    }

editedOpenIdClient :: OpenIdClient
editedOpenIdClient =
  defaultOpenIdClient
    { name = defaultOpenIdClientChangeDto.name
    , url = defaultOpenIdClientChangeDto.url
    , clientId = defaultOpenIdClientChangeDto.clientId
    , clientSecret = defaultOpenIdClientChangeDto.clientSecret
    , parameters = defaultOpenIdClientChangeDto.parameters
    , style = defaultOpenIdClientChangeDto.style
    , registrationEnabled = defaultOpenIdClientChangeDto.registrationEnabled
    , scopeProfile = defaultOpenIdClientChangeDto.scopeProfile
    , scopeEmail = defaultOpenIdClientChangeDto.scopeEmail
    }

editedOpenIdClientDetailDto :: OpenIdClientDetailDTO
editedOpenIdClientDetailDto =
  OpenIdClientDetailDTO
    { uuid = editedOpenIdClient.uuid
    , name = editedOpenIdClient.name
    , url = editedOpenIdClient.url
    , clientId = editedOpenIdClient.clientId
    , clientSecret = editedOpenIdClient.clientSecret
    , parameters = editedOpenIdClient.parameters
    , style = editedOpenIdClient.style
    , registrationEnabled = editedOpenIdClient.registrationEnabled
    , scopeProfile = editedOpenIdClient.scopeProfile
    , scopeEmail = editedOpenIdClient.scopeEmail
    , tenantUuid = editedOpenIdClient.tenantUuid
    , createdAt = editedOpenIdClient.createdAt
    , updatedAt = editedOpenIdClient.updatedAt
    }
