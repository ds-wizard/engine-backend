module Shared.Locale.Database.Mapping.Locale.LocaleSuggestion where

import Database.PostgreSQL.Simple

import Shared.Locale.Model.Locale.LocaleSuggestion

instance FromRow LocaleSuggestion
