module Wizard.Model.QuestionnaireAction.QuestionnaireAction where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.KnowledgeModel.Model.Package.PackagePattern

data QuestionnaireAction = QuestionnaireAction
  { qaId :: String
  , name :: String
  , organizationId :: String
  , actionId :: String
  , version :: String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , url :: String
  , config :: Object
  , enabled :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
