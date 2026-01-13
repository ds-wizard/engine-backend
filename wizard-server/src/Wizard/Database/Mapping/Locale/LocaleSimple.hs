module Wizard.Database.Mapping.Locale.LocaleSimple where

import Database.PostgreSQL.Simple

import Shared.Locale.Model.Locale.LocaleSimple

instance FromRow LocaleSimple
