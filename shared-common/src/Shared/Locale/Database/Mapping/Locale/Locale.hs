module Shared.Locale.Database.Mapping.Locale.Locale where

import Database.PostgreSQL.Simple

import Shared.Locale.Model.Locale.Locale

instance ToRow Locale

instance FromRow Locale
