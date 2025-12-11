module WizardLib.Public.Model.PersistentCommand.Project.CreateProjectCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateProjectCommand = CreateProjectCommand
  { name :: String
  , emails :: [String]
  , knowledgeModelPackageId :: String
  , documentTemplateId :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateProjectCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateProjectCommand where
  toJSON = genericToJSON jsonOptions
