module Wizard.Database.Mapping.Package.PackageState where

import Database.PostgreSQL.Simple.FromField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Package.PackageState

instance FromField PackageState where
  fromField = fromFieldGenericEnum
