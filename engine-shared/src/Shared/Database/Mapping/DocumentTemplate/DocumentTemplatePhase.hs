module Shared.Database.Mapping.DocumentTemplate.DocumentTemplatePhase where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.DocumentTemplate.DocumentTemplate

instance ToField DocumentTemplatePhase where
  toField = toFieldGenericEnum

instance FromField DocumentTemplatePhase where
  fromField = fromFieldGenericEnum
