module Wizard.Model.Questionnaire.QuestionnaireList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireState

data QuestionnaireList = QuestionnaireList
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , state :: QuestionnaireState
  , knowledgeModelPackage :: KnowledgeModelPackageSimple
  , permissions :: [QuestionnairePermDTO]
  , isTemplate :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq QuestionnaireList where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.description == b.description
      && a.visibility == b.visibility
      && a.sharing == b.sharing
      && a.state == b.state
      && a.knowledgeModelPackage == b.knowledgeModelPackage
      && a.permissions == b.permissions
      && a.isTemplate == b.isTemplate
