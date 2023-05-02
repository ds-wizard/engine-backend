module Wizard.Database.Mapping.Locale.LocaleList where

import Database.PostgreSQL.Simple

import Wizard.Database.Mapping.Locale.LocaleState ()
import Wizard.Model.Locale.LocaleList

instance FromRow LocaleList
