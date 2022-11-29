module Wizard.Database.Mapping.Locale.LocaleState where

import Database.PostgreSQL.Simple.FromField

import Shared.Database.Mapping.Common
import Wizard.Model.Locale.LocaleState

instance FromField LocaleState where
  fromField = fromFieldGenericEnum
