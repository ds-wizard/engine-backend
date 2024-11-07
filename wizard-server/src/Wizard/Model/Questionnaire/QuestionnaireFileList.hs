module Wizard.Model.Questionnaire.QuestionnaireFileList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

import Wizard.Model.Questionnaire.QuestionnaireSimple
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

data QuestionnaireFileList = QuestionnaireFileList
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  , questionnaire :: QuestionnaireSimple
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
