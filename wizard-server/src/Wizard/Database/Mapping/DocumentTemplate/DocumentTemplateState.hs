module Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateState where

import Database.PostgreSQL.Simple.FromField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.DocumentTemplate.DocumentTemplateState

instance FromField DocumentTemplateState where
  fromField = fromFieldGenericEnum
