module Wizard.Model.PersistentCommand.Questionnaire.File.QuestionnaireFileDeleteFromS3Command where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data QuestionnaireFileDeleteFromS3Command = QuestionnaireFileDeleteFromS3Command
  { questionnaireUuid :: U.UUID
  , fileUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON QuestionnaireFileDeleteFromS3Command where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireFileDeleteFromS3Command where
  toJSON = genericToJSON jsonOptions
