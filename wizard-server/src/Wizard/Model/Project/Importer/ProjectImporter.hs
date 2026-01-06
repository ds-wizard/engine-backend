module Wizard.Model.Project.Importer.ProjectImporter where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data ProjectImporter = ProjectImporter
  { piId :: String
  , name :: String
  , organizationId :: String
  , importerId :: String
  , version :: String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , url :: String
  , enabled :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
