module WizardLib.KnowledgeModel.Database.Mapping.Package.PackagePhase where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import WizardLib.KnowledgeModel.Model.Package.Package

instance ToField PackagePhase where
  toField = toFieldGenericEnum

instance FromField PackagePhase where
  fromField = fromFieldGenericEnum
