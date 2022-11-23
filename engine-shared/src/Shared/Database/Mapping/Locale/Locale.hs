module Shared.Database.Mapping.Locale.Locale where

import Database.PostgreSQL.Simple

import Shared.Model.Locale.Locale

instance ToRow Locale

instance FromRow Locale
