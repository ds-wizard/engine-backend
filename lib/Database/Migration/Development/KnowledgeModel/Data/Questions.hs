module Database.Migration.Development.KnowledgeModel.Data.Questions where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
       as FA
import Database.Migration.Development.KnowledgeModel.Data.Experts
       as FE
import Database.Migration.Development.KnowledgeModel.Data.References
       as FR
import Database.Migration.Development.KnowledgeModel.Data.Tags
       as FT
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

question1' :: Question
question1' = ValueQuestion' question1

question1 :: ValueQuestion
question1 =
  ValueQuestion
  { _valueQuestionUuid = fromJust $ U.fromString "2be1d749-9c72-4807-9309-d6c7bdbf13ba"
  , _valueQuestionTitle = "First Question"
  , _valueQuestionText = Just "Here is a description of question"
  , _valueQuestionRequiredLevel = Just 1
  , _valueQuestionTagUuids = [FT.tagDataScience ^. uuid]
  , _valueQuestionReferences = []
  , _valueQuestionExperts = []
  , _valueQuestionValueType = StringQuestionValueType
  }

question1WithChangeProperties' :: Question
question1WithChangeProperties' = ValueQuestion' question1WithChangeProperties

question1WithChangeProperties :: ValueQuestion
question1WithChangeProperties =
  ValueQuestion
  { _valueQuestionUuid = question1 ^. uuid
  , _valueQuestionTitle = "EDITED: " ++ question1 ^. title
  , _valueQuestionText = question1 ^. text
  , _valueQuestionRequiredLevel = question1 ^. requiredLevel
  , _valueQuestionTagUuids = question1 ^. tagUuids
  , _valueQuestionReferences = question1 ^. references
  , _valueQuestionExperts = question1 ^. experts
  , _valueQuestionValueType = question1 ^. valueType
  }

question1WithNewType' :: Question
question1WithNewType' = ListQuestion' question1WithNewType

question1WithNewType :: ListQuestion
question1WithNewType =
  ListQuestion
  { _listQuestionUuid = question1 ^. uuid
  , _listQuestionTitle = "EDITED: " ++ question1 ^. title
  , _listQuestionText = question1 ^. text
  , _listQuestionRequiredLevel = question1 ^. requiredLevel
  , _listQuestionTagUuids = question1 ^. tagUuids
  , _listQuestionReferences = question1 ^. references
  , _listQuestionExperts = question1 ^. experts
  , _listQuestionItemTemplateTitle = "Some Item Template Title"
  , _listQuestionItemTemplateQuestions = []
  }

-- -----------------------------------
question2' :: Question
question2' = OptionsQuestion' question2

question2 :: OptionsQuestion
question2 =
  OptionsQuestion
  { _optionsQuestionUuid = fromJust $ U.fromString "549d4140-d3e7-4cda-a373-7af8abc6325c"
  , _optionsQuestionTitle = "Is there any pre-existing data?"
  , _optionsQuestionText =
      Just "Are there any data sets available in the world that are relevant to your planned research?"
  , _optionsQuestionRequiredLevel = Just 2
  , _optionsQuestionTagUuids = [FT.tagBioInformatic ^. uuid]
  , _optionsQuestionReferences = [FR.referenceCh1', FR.referenceCh2']
  , _optionsQuestionExperts = [FE.expertAlbert, FE.expertNikola]
  , _optionsQuestionAnswers = [FA.q2_answerNo, FA.q2_answerYes]
  }

question2WithChangeProperties' :: Question
question2WithChangeProperties' = OptionsQuestion' question2WithChangeProperties

question2WithChangeProperties :: OptionsQuestion
question2WithChangeProperties =
  OptionsQuestion
  { _optionsQuestionUuid = question2 ^. uuid
  , _optionsQuestionTitle = "EDITED: Second Question"
  , _optionsQuestionText = Just "EDITED: Some long description"
  , _optionsQuestionRequiredLevel = Just 3
  , _optionsQuestionTagUuids = [FT.tagDataScience ^. uuid, FT.tagBioInformatic ^. uuid]
  , _optionsQuestionReferences = [FR.referenceCh2', FR.referenceCh1']
  , _optionsQuestionExperts = [FE.expertNikola, FE.expertAlbert]
  , _optionsQuestionAnswers = [FA.q2_answerYes, FA.q2_answerNo]
  }

question2WithNewType' :: Question
question2WithNewType' = ValueQuestion' question2WithNewType

question2WithNewType :: ValueQuestion
question2WithNewType =
  ValueQuestion
  { _valueQuestionUuid = question2 ^. uuid
  , _valueQuestionTitle = "EDITED: Second Question"
  , _valueQuestionText = question2 ^. text
  , _valueQuestionRequiredLevel = question2 ^. requiredLevel
  , _valueQuestionTagUuids = question2 ^. tagUuids
  , _valueQuestionReferences = question2 ^. references
  , _valueQuestionExperts = question2 ^. experts
  , _valueQuestionValueType = DateQuestionValueType
  }

question2Plain' :: Question
question2Plain' = OptionsQuestion' question2Plain

question2Plain :: OptionsQuestion
question2Plain =
  OptionsQuestion
  { _optionsQuestionUuid = question2 ^. uuid
  , _optionsQuestionTitle = question2 ^. title
  , _optionsQuestionText = question2 ^. text
  , _optionsQuestionRequiredLevel = question2 ^. requiredLevel
  , _optionsQuestionTagUuids = question2 ^. tagUuids
  , _optionsQuestionReferences = []
  , _optionsQuestionExperts = []
  , _optionsQuestionAnswers = []
  }

question3' :: Question
question3' = OptionsQuestion' question3

question3 :: OptionsQuestion
question3 =
  OptionsQuestion
  { _optionsQuestionUuid = fromJust $ U.fromString "b12d5939-2bd5-42b3-af09-a189480014d9"
  , _optionsQuestionTitle = "Third Question"
  , _optionsQuestionText = Just "Some long description"
  , _optionsQuestionRequiredLevel = Just 2
  , _optionsQuestionTagUuids = []
  , _optionsQuestionReferences = []
  , _optionsQuestionExperts = []
  , _optionsQuestionAnswers = [FA.q3_answerNo, FA.q3_answerYes]
  }

question3Plain' :: Question
question3Plain' = OptionsQuestion' question3Plain

question3Plain :: OptionsQuestion
question3Plain =
  OptionsQuestion
  { _optionsQuestionUuid = question3 ^. uuid
  , _optionsQuestionTitle = "Third Question"
  , _optionsQuestionText = Just "Some long description"
  , _optionsQuestionRequiredLevel = Just 2
  , _optionsQuestionTagUuids = []
  , _optionsQuestionReferences = []
  , _optionsQuestionExperts = []
  , _optionsQuestionAnswers = []
  }

question4' :: Question
question4' = ListQuestion' question4

question4 :: ListQuestion
question4 =
  ListQuestion
  { _listQuestionUuid = fromJust $ U.fromString "5c995368-b8dc-49e2-9b38-b79d4eb2779b"
  , _listQuestionTitle = "Fourth Question"
  , _listQuestionText = Just "Some nice description"
  , _listQuestionRequiredLevel = Nothing
  , _listQuestionTagUuids = [FT.tagBioInformatic ^. uuid]
  , _listQuestionReferences = []
  , _listQuestionExperts = []
  , _listQuestionItemTemplateTitle = "Template Title"
  , _listQuestionItemTemplateQuestions = [q4_it1_question5', q4_it1_question6']
  }

question4WithChangeProperties' :: Question
question4WithChangeProperties' = ListQuestion' question4WithChangeProperties

question4WithChangeProperties :: ListQuestion
question4WithChangeProperties =
  ListQuestion
  { _listQuestionUuid = question4 ^. uuid
  , _listQuestionTitle = "EDITED: " ++ question4 ^. title
  , _listQuestionText = Just $ "EDITED: " ++ (fromJust $ question4 ^. text)
  , _listQuestionRequiredLevel = Just 2
  , _listQuestionTagUuids = question4 ^. tagUuids
  , _listQuestionReferences = question4 ^. references
  , _listQuestionExperts = question4 ^. experts
  , _listQuestionItemTemplateTitle = "EDITED: Template Title"
  , _listQuestionItemTemplateQuestions = [q4_it1_question6', q4_it1_question5']
  }

question4WithNewType' :: Question
question4WithNewType' = OptionsQuestion' question4WithNewType

question4WithNewType :: OptionsQuestion
question4WithNewType =
  OptionsQuestion
  { _optionsQuestionUuid = question4 ^. uuid
  , _optionsQuestionTitle = "EDITED: Third Question"
  , _optionsQuestionText = question4 ^. text
  , _optionsQuestionRequiredLevel = question4 ^. requiredLevel
  , _optionsQuestionTagUuids = question4 ^. tagUuids
  , _optionsQuestionReferences = question4 ^. references
  , _optionsQuestionExperts = question4 ^. experts
  , _optionsQuestionAnswers = []
  }

question4Plain' :: Question
question4Plain' = ListQuestion' question4Plain

question4Plain :: ListQuestion
question4Plain =
  ListQuestion
  { _listQuestionUuid = question4 ^. uuid
  , _listQuestionTitle = question4 ^. title
  , _listQuestionText = question4 ^. text
  , _listQuestionRequiredLevel = question4 ^. requiredLevel
  , _listQuestionTagUuids = question4 ^. tagUuids
  , _listQuestionReferences = question4 ^. references
  , _listQuestionExperts = question4 ^. experts
  , _listQuestionItemTemplateTitle = "Template Title"
  , _listQuestionItemTemplateQuestions = []
  }

q4_it1_question5' :: Question
q4_it1_question5' = ListQuestion' q4_it1_question5

q4_it1_question5 :: ListQuestion
q4_it1_question5 =
  ListQuestion
  { _listQuestionUuid = fromJust $ U.fromString "9f8b1681-f6dc-4fdb-8e38-018df91fd2bd"
  , _listQuestionTitle = "Fifth Question"
  , _listQuestionText = Just "Some funny description"
  , _listQuestionRequiredLevel = Just 2
  , _listQuestionTagUuids = [FT.tagBioInformatic ^. uuid]
  , _listQuestionReferences = []
  , _listQuestionExperts = []
  , _listQuestionItemTemplateTitle = "Template Title 2"
  , _listQuestionItemTemplateQuestions = [q4_it1_q5_it2_question7', q4_it1_q5_it2_question8']
  }

q4_it1_question5Plain' :: Question
q4_it1_question5Plain' = ListQuestion' q4_it1_question5Plain

q4_it1_question5Plain :: ListQuestion
q4_it1_question5Plain =
  ListQuestion
  { _listQuestionUuid = q4_it1_question5 ^. uuid
  , _listQuestionTitle = q4_it1_question5 ^. title
  , _listQuestionText = q4_it1_question5 ^. text
  , _listQuestionRequiredLevel = Just 2
  , _listQuestionTagUuids = q4_it1_question5 ^. tagUuids
  , _listQuestionReferences = q4_it1_question5 ^. references
  , _listQuestionExperts = q4_it1_question5 ^. experts
  , _listQuestionItemTemplateTitle = q4_it1_question5 ^. itemTemplateTitle
  , _listQuestionItemTemplateQuestions = []
  }

q4_it1_question5Changed' :: Question
q4_it1_question5Changed' = ListQuestion' q4_it1_question5Changed

q4_it1_question5Changed :: ListQuestion
q4_it1_question5Changed =
  ListQuestion
  { _listQuestionUuid = q4_it1_question5 ^. uuid
  , _listQuestionTitle = "EDITED: Fifth Question"
  , _listQuestionText = Just "EDITED: Some funny description"
  , _listQuestionRequiredLevel = Just 3
  , _listQuestionTagUuids = q4_it1_question5 ^. tagUuids
  , _listQuestionReferences = q4_it1_question5 ^. references
  , _listQuestionExperts = q4_it1_question5 ^. experts
  , _listQuestionItemTemplateTitle = "EDITED: Template Title 2"
  , _listQuestionItemTemplateQuestions = [q4_it1_q5_it2_question8', q4_it1_q5_it2_question7']
  }

q4_it1_question6' :: Question
q4_it1_question6' = OptionsQuestion' q4_it1_question6

q4_it1_question6 :: OptionsQuestion
q4_it1_question6 =
  OptionsQuestion
  { _optionsQuestionUuid = fromJust $ U.fromString "efcf425f-f5c6-4c36-9aaf-fd4ced17adf5"
  , _optionsQuestionTitle = "Sixth Question"
  , _optionsQuestionText = Just "Some non-funny description"
  , _optionsQuestionRequiredLevel = Just 2
  , _optionsQuestionTagUuids = []
  , _optionsQuestionReferences = [FR.referenceCh1', FR.referenceCh2']
  , _optionsQuestionExperts = [FE.expertAlbert, FE.expertNikola]
  , _optionsQuestionAnswers = [q4_it1_q6_answerNo, q4_it1_q6_answerYes]
  }

q4_it1_question6Changed' :: Question
q4_it1_question6Changed' = OptionsQuestion' q4_it1_question6Changed

q4_it1_question6Changed :: OptionsQuestion
q4_it1_question6Changed =
  OptionsQuestion
  { _optionsQuestionUuid = q4_it1_question6 ^. uuid
  , _optionsQuestionTitle = "Sixth Question"
  , _optionsQuestionText = Just "Some non-funny description"
  , _optionsQuestionRequiredLevel = Nothing
  , _optionsQuestionTagUuids = []
  , _optionsQuestionReferences = [FR.referenceCh2', FR.referenceCh1']
  , _optionsQuestionExperts = [FE.expertNikola, FE.expertAlbert]
  , _optionsQuestionAnswers = [q4_it1_q6_answerYes, q4_it1_q6_answerNo]
  }

q4_it1_q5_it2_question7' :: Question
q4_it1_q5_it2_question7' = ValueQuestion' q4_it1_q5_it2_question7

q4_it1_q5_it2_question7 :: ValueQuestion
q4_it1_q5_it2_question7 =
  ValueQuestion
  { _valueQuestionUuid = fromJust $ U.fromString "385026a5-c35b-4461-9588-bcbc10c99ac5"
  , _valueQuestionTitle = "Seventh Question"
  , _valueQuestionText = Just "Some non-funny description"
  , _valueQuestionRequiredLevel = Just 2
  , _valueQuestionTagUuids = []
  , _valueQuestionReferences = []
  , _valueQuestionExperts = []
  , _valueQuestionValueType = StringQuestionValueType
  }

q4_it1_q5_it2_question8' :: Question
q4_it1_q5_it2_question8' = ValueQuestion' q4_it1_q5_it2_question8

q4_it1_q5_it2_question8 :: ValueQuestion
q4_it1_q5_it2_question8 =
  ValueQuestion
  { _valueQuestionUuid = fromJust $ U.fromString "f272a0b6-14fd-477f-8a95-d7ea483a4395"
  , _valueQuestionTitle = "Eighth Question"
  , _valueQuestionText = Just "Some non-funny description"
  , _valueQuestionRequiredLevel = Just 2
  , _valueQuestionTagUuids = []
  , _valueQuestionReferences = []
  , _valueQuestionExperts = []
  , _valueQuestionValueType = StringQuestionValueType
  }
