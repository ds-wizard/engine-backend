module Shared.Model.Template.TemplateGroup where

import GHC.Generics

import Shared.Model.Template.Template

data TemplateGroup = TemplateGroup
  { organizationId :: String
  , templateId :: String
  , versions :: [Template]
  }
  deriving (Show, Eq, Generic)
