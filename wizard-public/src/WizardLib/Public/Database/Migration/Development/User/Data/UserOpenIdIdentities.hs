module WizardLib.Public.Database.Migration.Development.User.Data.UserOpenIdIdentities where

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.OpenId.Database.Migration.Development.OpenId.Data.OpenIds
import WizardLib.Public.Api.Resource.User.UserOpenIdIdentityDTO
import WizardLib.Public.Database.Migration.Development.OpenId.Data.OpenIdClients
import WizardLib.Public.Model.OpenId.OpenIdClient

defaultUserOpenIdIdentityDto :: UserOpenIdIdentityDTO
defaultUserOpenIdIdentityDto =
  UserOpenIdIdentityDTO
    { uuid = u' "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
    , externalId = "albert.einstein@example.com"
    , externalLabel = Just "Albert"
    , providerUuid = (defaultOpenIdClient :: OpenIdClient).uuid
    , providerName = (defaultOpenIdClient :: OpenIdClient).name
    , providerStyle = openIdClientDefinitionStyle
    , createdAt = dt' 2018 1 21
    }
