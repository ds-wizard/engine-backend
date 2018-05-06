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
  , _questionQType = QuestionTypeOptions
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
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Second Question"
  , _questionText = "Some long description"
  , _questionAnswers = Just [FA.answerNo1, FA.answerYes1]
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = [FR.referenceCh1, FR.referenceCh2]
  , _questionExperts = [FE.expertAlbert, FE.expertNikola]
  }

question2WithChangeProperties :: Question
question2WithChangeProperties =
  Question
  { _questionUuid = question2 ^. uuid
  , _questionShortUuid = question2 ^. shortUuid
  , _questionQType = QuestionTypeString
  , _questionTitle = "EDITED: Second Question"
  , _questionText = "EDITED: Some long description"
  , _questionAnswerItemTemplate = Nothing
  , _questionAnswers = Just [FA.answerYes1, FA.answerNo1]
  , _questionReferences = [FR.referenceCh2, FR.referenceCh1]
  , _questionExperts = [FE.expertNikola, FE.expertAlbert]
  }

question3 :: Question
question3 =
  Question
  { _questionUuid = fromJust $ U.fromString "b12d5939-2bd5-42b3-af09-a189480014d9"
  , _questionShortUuid = Just "question3"
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Third Question"
  , _questionText = "Some long description"
  , _questionAnswerItemTemplate = Nothing
  , _questionAnswers = Just [FA.answerNo2, FA.answerYes2]
  , _questionReferences = []
  , _questionExperts = []
  }

question3Plain :: Question
question3Plain =
  Question
  { _questionUuid = question3 ^. uuid
  , _questionShortUuid = question3 ^. shortUuid
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Third Question"
  , _questionText = "Some long description"
  , _questionAnswers = Just []
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

ait1 :: AnswerItemTemplate
ait1 =
  AnswerItemTemplate
  {_answerItemTemplateTitle = "Template Title", _answerItemTemplateQuestions = [question5, question6]}

ait1WithChangeProperties :: AnswerItemTemplate
ait1WithChangeProperties =
  AnswerItemTemplate
  {_answerItemTemplateTitle = "EDITED: Template Title", _answerItemTemplateQuestions = [question6, question5]}

ait1Plain :: AnswerItemTemplate
ait1Plain = AnswerItemTemplate {_answerItemTemplateTitle = "Template Title", _answerItemTemplateQuestions = []}

ait2 :: AnswerItemTemplate
ait2 =
  AnswerItemTemplate
  {_answerItemTemplateTitle = "Template Title 2", _answerItemTemplateQuestions = [question7, question8]}

ait2WithChangeProperties :: AnswerItemTemplate
ait2WithChangeProperties =
  AnswerItemTemplate
  {_answerItemTemplateTitle = "EDITED: Template Title 2", _answerItemTemplateQuestions = [question8, question7]}

question4 :: Question
question4 =
  Question
  { _questionUuid = fromJust $ U.fromString "5c995368-b8dc-49e2-9b38-b79d4eb2779b"
  , _questionShortUuid = Just "question4"
  , _questionQType = QuestionTypeList
  , _questionTitle = "Fourth Question"
  , _questionText = "Some nice description"
  , _questionAnswerItemTemplate = Just ait1
  , _questionAnswers = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

question4WithChangeProperties :: Question
question4WithChangeProperties =
  Question
  { _questionUuid = fromJust $ U.fromString "5c995368-b8dc-49e2-9b38-b79d4eb2779b"
  , _questionShortUuid = Just "question4"
  , _questionQType = QuestionTypeList
  , _questionTitle = "EDITED: Fourth Question"
  , _questionText = "EDITED: Some nice description"
  , _questionAnswerItemTemplate = Just ait1WithChangeProperties
  , _questionAnswers = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

question4Plain :: Question
question4Plain =
  Question
  { _questionUuid = fromJust $ U.fromString "5c995368-b8dc-49e2-9b38-b79d4eb2779b"
  , _questionShortUuid = Just "question4"
  , _questionQType = QuestionTypeList
  , _questionTitle = "Fourth Question"
  , _questionText = "Some nice description"
  , _questionAnswers = Nothing
  , _questionAnswerItemTemplate = Just ait1Plain
  , _questionReferences = []
  , _questionExperts = []
  }

question5 :: Question
question5 =
  Question
  { _questionUuid = fromJust $ U.fromString "9f8b1681-f6dc-4fdb-8e38-018df91fd2bd"
  , _questionShortUuid = Just "question5"
  , _questionQType = QuestionTypeList
  , _questionTitle = "Fifth Question"
  , _questionText = "Some funny description"
  , _questionAnswers = Nothing
  , _questionAnswerItemTemplate = Just ait2
  , _questionReferences = []
  , _questionExperts = []
  }

question5Plain :: Question
question5Plain =
  Question
  { _questionUuid = question5 ^. uuid
  , _questionShortUuid = question5 ^. shortUuid
  , _questionQType = question5 ^. qType
  , _questionTitle = question5 ^. title
  , _questionText = question5 ^. text
  , _questionAnswerItemTemplate = Just (ait2 & questions .~ [])
  , _questionAnswers = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

question5WithChangeProperties :: Question
question5WithChangeProperties =
  Question
  { _questionUuid = question5 ^. uuid
  , _questionShortUuid = Just "question5"
  , _questionQType = QuestionTypeString
  , _questionTitle = "EDITED: Fifth Question"
  , _questionText = "EDITED: Some funny description"
  , _questionAnswerItemTemplate = Just ait2WithChangeProperties
  , _questionAnswers = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

question6 :: Question
question6 =
  Question
  { _questionUuid = fromJust $ U.fromString "efcf425f-f5c6-4c36-9aaf-fd4ced17adf5"
  , _questionShortUuid = Just "question6"
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Sixth Question"
  , _questionText = "Some non-funny description"
  , _questionAnswerItemTemplate = Nothing
  , _questionAnswers = Just [answerNo6, answerYes6]
  , _questionReferences = [FR.referenceCh1, FR.referenceCh2]
  , _questionExperts = [FE.expertAlbert, FE.expertNikola]
  }

question6WithChangeProperties :: Question
question6WithChangeProperties =
  Question
  { _questionUuid = question6 ^. uuid
  , _questionShortUuid = Just "question6"
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Sixth Question"
  , _questionText = "Some non-funny description"
  , _questionAnswerItemTemplate = Nothing
  , _questionAnswers = Just [answerYes6, answerNo6]
  , _questionReferences = [FR.referenceCh2, FR.referenceCh1]
  , _questionExperts = [FE.expertNikola, FE.expertAlbert]
  }

question7 :: Question
question7 =
  Question
  { _questionUuid = fromJust $ U.fromString "385026a5-c35b-4461-9588-bcbc10c99ac5"
  , _questionShortUuid = Just "question7"
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Seventh Question"
  , _questionText = "Some non-funny description"
  , _questionAnswerItemTemplate = Nothing
  , _questionAnswers = Just []
  , _questionReferences = []
  , _questionExperts = []
  }

question8 :: Question
question8 =
  Question
  { _questionUuid = fromJust $ U.fromString "f272a0b6-14fd-477f-8a95-d7ea483a4395"
  , _questionShortUuid = Just "question8"
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Eighth Question"
  , _questionText = "Some non-funny description"
  , _questionAnswerItemTemplate = Nothing
  , _questionAnswers = Just []
  , _questionReferences = []
  , _questionExperts = []
  }
