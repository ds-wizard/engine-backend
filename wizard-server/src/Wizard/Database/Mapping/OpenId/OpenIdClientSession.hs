module Wizard.Database.Mapping.OpenId.OpenIdClientSession where

import Database.PostgreSQL.Simple

import Shared.Common.Database.Mapping.Common ()
import Wizard.Model.OpenId.OpenIdClientSession

instance FromRow OpenIdClientSession

instance ToRow OpenIdClientSession
