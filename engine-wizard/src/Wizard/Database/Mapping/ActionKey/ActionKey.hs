module Wizard.Database.Mapping.ActionKey.ActionKey where

import Database.PostgreSQL.Simple

import Wizard.Database.Mapping.ActionKey.ActionKeyType ()
import Wizard.Model.ActionKey.ActionKey

instance ToRow ActionKey

instance FromRow ActionKey
