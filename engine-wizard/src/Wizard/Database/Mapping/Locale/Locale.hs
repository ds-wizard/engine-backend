module Wizard.Database.Mapping.Locale.Locale where

import Database.PostgreSQL.Simple

import Wizard.Model.Locale.Locale

instance ToRow Locale

instance FromRow Locale
