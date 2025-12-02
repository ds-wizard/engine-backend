module Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageGroup where

import GHC.Generics

data KnowledgeModelPackageGroup = KnowledgeModelPackageGroup
  { organizationId :: String
  , kmId :: String
  , versions :: String
  }
  deriving (Show, Eq, Generic)
