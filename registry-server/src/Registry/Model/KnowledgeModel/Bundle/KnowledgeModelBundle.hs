module Registry.Model.KnowledgeModel.Bundle.KnowledgeModelBundle where

import GHC.Generics

import Registry.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw

data KnowledgeModelBundle = KnowledgeModelBundle
  { bundleId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [KnowledgeModelPackageRaw]
  }
  deriving (Show, Eq, Generic)
