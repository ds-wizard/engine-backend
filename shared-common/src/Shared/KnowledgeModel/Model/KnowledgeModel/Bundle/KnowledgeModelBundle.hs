module Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle where

import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage

data KnowledgeModelBundle = KnowledgeModelBundle
  { bundleId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [KnowledgeModelBundlePackage]
  }
  deriving (Show, Eq, Generic)
