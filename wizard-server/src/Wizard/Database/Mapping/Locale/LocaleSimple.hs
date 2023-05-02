module Wizard.Database.Mapping.Locale.LocaleSimple where

import Database.PostgreSQL.Simple

import Wizard.Model.Locale.LocaleSimple

instance FromRow LocaleSimple
