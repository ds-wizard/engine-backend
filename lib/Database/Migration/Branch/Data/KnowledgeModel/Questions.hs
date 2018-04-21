module Database.Migration.Branch.Data.KnowledgeModel.Questions where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.Branch.Data.KnowledgeModel.AnswersAndFollowUpQuestions
       as FA
import Database.Migration.Branch.Data.KnowledgeModel.Experts as FE
import Database.Migration.Branch.Data.KnowledgeModel.References
       as FR
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

question1 :: Question
question1 =
  Question
  { _questionUuid = fromJust $ U.fromString "2be1d749-9c72-4807-9309-d6c7bdbf13ba"
  , _questionShortUuid = Just "question1"
  , _questionQType = QuestionTypeOption
  , _questionTitle = "First Question"
  , _questionText = "Here is a description of question"
  , _questionAnswers = Just []
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

-- -----------------------------------
question2 :: Question
question2 =
  Question
  { _questionUuid = fromJust $ U.fromString "549d4140-d3e7-4cda-a373-7af8abc6325c"
  , _questionShortUuid = Just "question2"
  , _questionQType = QuestionTypeOption
  , _questionTitle = "Second Question"
  , _questionText = "Some long description"
  , _questionAnswers = Just [FA.answerNo1, FA.answerYes1]
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = [FR.referenceCh1, FR.referenceCh2]
  , _questionExperts = [FE.expertDarth, FE.expertLuke]
  }

question2WithChangeProperties :: Question
question2WithChangeProperties =
  Question
  { _questionUuid = question2 ^. uuid
  , _questionShortUuid = question2 ^. shortUuid
  , _questionQType = QuestionString
  , _questionTitle = "EDITED: Second Question"
  , _questionText = "EDITED: Some long description"
  , _questionAnswers = Just [FA.answerYes1, FA.answerNo1]
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = [FR.referenceCh2, FR.referenceCh1]
  , _questionExperts = [FE.expertLuke, FE.expertDarth]
  }

question3 :: Question
question3 =
  Question
  { _questionUuid = fromJust $ U.fromString "b12d5939-2bd5-42b3-af09-a189480014d9"
  , _questionShortUuid = Just "question3"
  , _questionQType = QuestionTypeOption
  , _questionTitle = "Third Question"
  , _questionText = "Some long description"
  , _questionAnswers = Just [FA.answerNo2, FA.answerYes2]
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

question3Plain :: Question
question3Plain =
  Question
  { _questionUuid = question3 ^. uuid
  , _questionShortUuid = question3 ^. shortUuid
  , _questionQType = QuestionTypeOption
  , _questionTitle = "Third Question"
  , _questionText = "Some long description"
  , _questionAnswers = Just []
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }
