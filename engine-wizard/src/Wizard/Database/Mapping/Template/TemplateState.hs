module Wizard.Database.Mapping.Template.TemplateState where

import Database.PostgreSQL.Simple.FromField

import Shared.Database.Mapping.Common
import Wizard.Model.Template.TemplateState

instance FromField TemplateState where
  fromField = fromFieldGenericEnum
