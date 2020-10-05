module Shared.Model.Template.TemplateGroup where

import GHC.Generics

import Shared.Model.Template.Template

data TemplateGroup =
  TemplateGroup
    { _templateGroupOrganizationId :: String
    , _templateGroupTemplateId :: String
    , _templateGroupVersions :: [Template]
    }
  deriving (Show, Eq, Generic)
