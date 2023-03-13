module Shared.Database.Mapping.Package.PackagePhase where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.Package.Package

instance ToField PackagePhase where
  toField = toFieldGenericEnum

instance FromField PackagePhase where
  fromField = fromFieldGenericEnum
