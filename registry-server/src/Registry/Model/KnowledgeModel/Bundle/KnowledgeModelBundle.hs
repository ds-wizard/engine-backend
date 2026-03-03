module Registry.Model.KnowledgeModel.Bundle.KnowledgeModelBundle where

import GHC.Generics

import Registry.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw
import Shared.Coordinate.Model.Coordinate.Coordinate

data KnowledgeModelBundle = KnowledgeModelBundle
  { bundleId :: Coordinate
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [KnowledgeModelPackageRaw]
  }
  deriving (Show, Eq, Generic)
