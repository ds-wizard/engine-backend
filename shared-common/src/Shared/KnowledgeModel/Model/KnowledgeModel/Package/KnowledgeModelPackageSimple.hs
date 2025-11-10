module Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple where

import GHC.Generics

data KnowledgeModelPackageSimple = KnowledgeModelPackageSimple
  { pId :: String
  , name :: String
  , version :: String
  }
  deriving (Generic, Eq, Show)
