module Fixtures.KnowledgeModel.Questions where

import Fixtures.KnowledgeModel.AnswersAndFollowUpQuestions as FA
import Fixtures.KnowledgeModel.Common as FC
import Fixtures.KnowledgeModel.Experts as FE
import Fixtures.KnowledgeModel.References as FR
import KMMigration.Model.KnowledgeModel

qTypeOption = "option"

qTypeList = "list"

question1 =
  Question
  { _qUuid = "question1"
  , _qType = qTypeOption
  , _qTitle = "First Question"
  , _qText = "Here is a description of question"
  , _qAnswers = []
  , _qReferences = []
  , _qExperts = []
  }

-- -----------------------------------
question2 =
  Question
  { _qUuid = "question2"
  -- , _qNamespace = FC.namespaceCore
  , _qType = qTypeOption
  , _qTitle = "Second Question"
  , _qText = "Some long description"
  , _qAnswers = [FA.answerNo1, FA.answerYes1]
  , _qReferences = [FR.referenceCh1, referenceCh2]
  , _qExperts = [FE.expertDarth, FE.expertLuke]
  }

question2WithChangeProperties =
  Question
  { _qUuid = "question2"
  -- , _qNamespace = FC.namespaceCore
  , _qType = qTypeList
  , _qTitle = "EDITED: Second Question"
  , _qText = "EDITED: Some long description"
  , _qAnswers = [FA.answerYes1, FA.answerNo1]
  , _qReferences = [FR.referenceCh2, FR.referenceCh1]
  , _qExperts = [FE.expertLuke, FE.expertDarth]
  }

question3 =
  Question
  { _qUuid = "question3"
  -- , _qNamespace = FC.namespaceCore
  , _qType = qTypeOption
  , _qTitle = "Third Question"
  , _qText = "Some long description"
  , _qAnswers = [FA.answerNo2, FA.answerYes2]
  , _qReferences = []
  , _qExperts = []
  }

question3Plain =
  Question
  { _qUuid = "question3"
  -- , _qNamespace = FC.namespaceCore
  , _qType = qTypeOption
  , _qTitle = "Third Question"
  , _qText = "Some long description"
  , _qAnswers = []
  , _qReferences = []
  , _qExperts = []
  }

