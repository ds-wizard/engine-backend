module Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern where

import GHC.Generics

data KnowledgeModelPackagePattern = KnowledgeModelPackagePattern
  { orgId :: Maybe String
  , kmId :: Maybe String
  , minVersion :: Maybe String
  , maxVersion :: Maybe String
  }
  deriving (Show, Eq, Generic)
