module Wizard.Model.Questionnaire.QuestionnaireDetail where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireState
import WizardLib.KnowledgeModel.Model.Package.PackageSimple

data QuestionnaireDetail = QuestionnaireDetail
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , state :: QuestionnaireState
  , package :: PackageSimple
  , permissions :: [QuestionnairePermRecordDTO]
  , isTemplate :: Bool
  , answeredQuestions :: Int
  , unansweredQuestions :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq QuestionnaireDetail where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.description == b.description
      && a.visibility == b.visibility
      && a.sharing == b.sharing
      && a.state == b.state
      && a.package == b.package
      && a.permissions == b.permissions
      && a.isTemplate == b.isTemplate
      && a.answeredQuestions == b.answeredQuestions
      && a.unansweredQuestions == b.unansweredQuestions
