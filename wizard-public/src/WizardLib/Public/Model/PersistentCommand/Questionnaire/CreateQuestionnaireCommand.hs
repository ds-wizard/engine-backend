module WizardLib.Public.Model.PersistentCommand.Questionnaire.CreateQuestionnaireCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateQuestionnaireCommand = CreateQuestionnaireCommand
  { name :: String
  , emails :: [String]
  , knowledgeModelPackageId :: String
  , documentTemplateId :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateQuestionnaireCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateQuestionnaireCommand where
  toJSON = genericToJSON jsonOptions
