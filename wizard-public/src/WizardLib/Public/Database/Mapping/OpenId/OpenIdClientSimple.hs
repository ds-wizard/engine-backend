module WizardLib.Public.Database.Mapping.OpenId.OpenIdClientSimple where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import WizardLib.Public.Model.OpenId.OpenIdClientSimple

instance FromRow OpenIdClientSimple where
  fromRow = do
    uuid <- field
    name <- field
    url <- field
    style <- fieldWith fromJSONField
    return $ OpenIdClientSimple {..}
