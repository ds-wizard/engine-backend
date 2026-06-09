module Wizard.Service.OpenId.Client.Definition.OpenIdClientDefinitionMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import WizardLib.Public.Model.OpenId.OpenIdClient
import WizardLib.Public.Model.PersistentCommand.OpenId.CreateOrUpdateOpenIdClientDefinitionCommand

fromCreate :: CreateOrUpdateOpenIdClientDefinitionCommand -> U.UUID -> UTCTime -> OpenIdClient
fromCreate command tenantUuid now =
  OpenIdClient
    { uuid = command.uuid
    , name = command.name
    , url = command.url
    , clientId = command.clientId
    , clientSecret = command.clientSecret
    , parameters = []
    , style = OpenIdClientStyle {icon = Nothing, background = Nothing, color = Nothing}
    , registrationEnabled = False
    , scopeProfile = False
    , scopeEmail = False
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromUpdate :: OpenIdClient -> CreateOrUpdateOpenIdClientDefinitionCommand -> UTCTime -> OpenIdClient
fromUpdate oldOpenIdClient command now =
  oldOpenIdClient
    { name = command.name
    , url = command.url
    , clientId = command.clientId
    , clientSecret = command.clientSecret
    , updatedAt = now
    }
