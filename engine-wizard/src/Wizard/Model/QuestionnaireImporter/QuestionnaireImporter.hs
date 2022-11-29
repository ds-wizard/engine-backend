module Wizard.Model.QuestionnaireImporter.QuestionnaireImporter where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Package.PackagePattern

data QuestionnaireImporter = QuestionnaireImporter
  { qiId :: String
  , name :: String
  , organizationId :: String
  , importerId :: String
  , version :: String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , url :: String
  , enabled :: Bool
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
