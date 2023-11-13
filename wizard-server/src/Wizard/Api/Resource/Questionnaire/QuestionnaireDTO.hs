module Wizard.Api.Resource.Questionnaire.QuestionnaireDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireState
import WizardLib.KnowledgeModel.Model.Package.PackageSimple

data QuestionnaireDTO = QuestionnaireDTO
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , state :: QuestionnaireState
  , package :: PackageSimple
  , answeredQuestions :: Int
  , unansweredQuestions :: Int
  , permissions :: [QuestionnairePermDTO]
  , isTemplate :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq QuestionnaireDTO where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.description == b.description
      && a.visibility == b.visibility
      && a.sharing == b.sharing
      && a.state == b.state
      && a.package == b.package
      && a.answeredQuestions == b.answeredQuestions
      && a.unansweredQuestions == b.unansweredQuestions
      && a.permissions == b.permissions
      && a.isTemplate == b.isTemplate
