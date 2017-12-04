module Database.Migration.Branch.Data.KnowledgeModel.Questions where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.Branch.Data.KnowledgeModel.AnswersAndFollowUpQuestions
       as FA
import Database.Migration.Branch.Data.KnowledgeModel.Experts as FE
import Database.Migration.Branch.Data.KnowledgeModel.References
       as FR
import Model.KnowledgeModel.KnowledgeModel

qTypeOption = "option"

qTypeList = "list"

question1 :: Question
question1 =
  Question
  { _qUuid = fromJust $ U.fromString "2be1d749-9c72-4807-9309-d6c7bdbf13ba"
  , _qShortUuid = Just "question1"
  , _qType = qTypeOption
  , _qTitle = "First Question"
  , _qText = "Here is a description of question"
  , _qAnswers = []
  , _qReferences = []
  , _qExperts = []
  }

-- -----------------------------------
question2 :: Question
question2 =
  Question
  { _qUuid = fromJust $ U.fromString "549d4140-d3e7-4cda-a373-7af8abc6325c"
  , _qShortUuid = Just "question2"
  , _qType = qTypeOption
  , _qTitle = "Second Question"
  , _qText = "Some long description"
  , _qAnswers = [FA.answerNo1, FA.answerYes1]
  , _qReferences = [FR.referenceCh1, FR.referenceCh2]
  , _qExperts = [FE.expertDarth, FE.expertLuke]
  }

question2WithChangeProperties :: Question
question2WithChangeProperties =
  Question
  { _qUuid = question2 ^. qUuid
  , _qShortUuid = question2 ^. qShortUuid
  , _qType = qTypeList
  , _qTitle = "EDITED: Second Question"
  , _qText = "EDITED: Some long description"
  , _qAnswers = [FA.answerYes1, FA.answerNo1]
  , _qReferences = [FR.referenceCh2, FR.referenceCh1]
  , _qExperts = [FE.expertLuke, FE.expertDarth]
  }

question3 :: Question
question3 =
  Question
  { _qUuid = fromJust $ U.fromString "b12d5939-2bd5-42b3-af09-a189480014d9"
  , _qShortUuid = Just "question3"
  , _qType = qTypeOption
  , _qTitle = "Third Question"
  , _qText = "Some long description"
  , _qAnswers = [FA.answerNo2, FA.answerYes2]
  , _qReferences = []
  , _qExperts = []
  }

question3Plain :: Question
question3Plain =
  Question
  { _qUuid = question3 ^. qUuid
  , _qShortUuid = question3 ^. qShortUuid
  , _qType = qTypeOption
  , _qTitle = "Third Question"
  , _qText = "Some long description"
  , _qAnswers = []
  , _qReferences = []
  , _qExperts = []
  }
