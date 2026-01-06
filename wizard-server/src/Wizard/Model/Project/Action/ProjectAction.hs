module Wizard.Model.Project.Action.ProjectAction where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data ProjectAction = ProjectAction
  { paId :: String
  , name :: String
  , organizationId :: String
  , actionId :: String
  , version :: String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , url :: String
  , config :: Object
  , enabled :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
