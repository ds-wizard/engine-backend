module Model.KnowledgeModelBundle.KnowledgeModelBundle where

import GHC.Generics

import Model.Package.Package

data KnowledgeModelBundle = KnowledgeModelBundle
  { _knowledgeModelBundleBundleId :: String
  , _knowledgeModelBundleName :: String
  , _knowledgeModelBundleOrganizationId :: String
  , _knowledgeModelBundleKmId :: String
  , _knowledgeModelBundleVersion :: String
  , _knowledgeModelBundleMetamodelVersion :: Int
  , _knowledgeModelBundlePackages :: [PackageWithEvents]
  } deriving (Show, Eq, Generic)
