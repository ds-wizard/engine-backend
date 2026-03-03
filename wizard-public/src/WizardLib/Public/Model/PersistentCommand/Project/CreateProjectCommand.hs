module WizardLib.Public.Model.PersistentCommand.Project.CreateProjectCommand where

import Data.Aeson
import GHC.Generics

import qualified Data.UUID as U
import Shared.Common.Util.Aeson

data CreateProjectCommand = CreateProjectCommand
  { name :: String
  , emails :: [String]
  , knowledgeModelPackageUuid :: U.UUID
  , documentTemplateUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateProjectCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateProjectCommand where
  toJSON = genericToJSON jsonOptions
