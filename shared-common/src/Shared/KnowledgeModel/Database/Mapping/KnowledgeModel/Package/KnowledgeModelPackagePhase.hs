module Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToField KnowledgeModelPackagePhase where
  toField = toFieldGenericEnum

instance FromField KnowledgeModelPackagePhase where
  fromField = fromFieldGenericEnum
