module Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle where

import GHC.Generics

import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage

data KnowledgeModelBundle = KnowledgeModelBundle
  { bundleId :: Coordinate
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [KnowledgeModelBundlePackage]
  }
  deriving (Show, Eq, Generic)

instance CoordinateFactory KnowledgeModelBundle where
  createCoordinate p =
    Coordinate
      { organizationId = p.organizationId
      , entityId = p.kmId
      , version = p.version
      }
