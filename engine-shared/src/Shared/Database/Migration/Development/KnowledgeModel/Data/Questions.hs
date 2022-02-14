module Shared.Database.Migration.Development.KnowledgeModel.Data.Questions where

import Control.Lens
import qualified Data.Map as Map
import Data.Maybe

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Choices
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

question1' :: Question
question1' = ValueQuestion' question1

question1 :: ValueQuestion
question1 =
  ValueQuestion
    { _valueQuestionUuid = u' "00000000-0000-0000-0000-0000000000f1"
    , _valueQuestionTitle = "First Question"
    , _valueQuestionText = Just "Here is a description of question"
    , _valueQuestionRequiredPhaseUuid = Just $ phase1 ^. uuid
    , _valueQuestionAnnotations = []
    , _valueQuestionTagUuids = [tagDataScience ^. uuid, tagBioInformatic ^. uuid]
    , _valueQuestionReferenceUuids = []
    , _valueQuestionExpertUuids = []
    , _valueQuestionValueType = StringQuestionValueType
    }

question1Edited' :: Question
question1Edited' = ValueQuestion' question1Edited

question1Edited :: ValueQuestion
question1Edited =
  ValueQuestion
    { _valueQuestionUuid = question1 ^. uuid
    , _valueQuestionTitle = "EDITED: " ++ question1 ^. title
    , _valueQuestionText = question1 ^. text
    , _valueQuestionRequiredPhaseUuid = question1 ^. requiredPhaseUuid
    , _valueQuestionAnnotations = question1 ^. annotations
    , _valueQuestionTagUuids = question1 ^. tagUuids
    , _valueQuestionReferenceUuids = question1 ^. referenceUuids
    , _valueQuestionExpertUuids = question1 ^. expertUuids
    , _valueQuestionValueType = question1 ^. valueType
    }

question1WithNewType' :: Question
question1WithNewType' = OptionsQuestion' question1WithNewType

question1WithNewType :: OptionsQuestion
question1WithNewType =
  OptionsQuestion
    { _optionsQuestionUuid = question1 ^. uuid
    , _optionsQuestionTitle = "EDITED: " ++ question1 ^. title
    , _optionsQuestionText = question1 ^. text
    , _optionsQuestionRequiredPhaseUuid = question1 ^. requiredPhaseUuid
    , _optionsQuestionAnnotations = question1 ^. annotations
    , _optionsQuestionTagUuids = question1 ^. tagUuids
    , _optionsQuestionReferenceUuids = question1 ^. referenceUuids
    , _optionsQuestionExpertUuids = question1 ^. expertUuids
    , _optionsQuestionAnswerUuids = []
    }

-- -----------------------------------
question2' :: Question
question2' = OptionsQuestion' question2

question2 :: OptionsQuestion
question2 =
  OptionsQuestion
    { _optionsQuestionUuid = u' "00000000-0000-0000-0000-0000000000f2"
    , _optionsQuestionTitle = "Is there any pre-existing data?"
    , _optionsQuestionText =
        Just "Are there any data sets available in the world that are relevant to your planned research?"
    , _optionsQuestionRequiredPhaseUuid = Just $ phase2 ^. uuid
    , _optionsQuestionAnnotations = []
    , _optionsQuestionTagUuids = [tagBioInformatic ^. uuid]
    , _optionsQuestionReferenceUuids = [km1_ch1_q2_r1 ^. uuid, km1_ch1_q2_r2 ^. uuid]
    , _optionsQuestionExpertUuids = [km1_ch1_q2_eAlbert ^. uuid, km1_ch1_q2_eNikola ^. uuid]
    , _optionsQuestionAnswerUuids = [q2_answerNo ^. uuid, q2_answerYes ^. uuid]
    }

question2Edited' :: Question
question2Edited' = OptionsQuestion' question2Edited

question2Edited :: OptionsQuestion
question2Edited =
  OptionsQuestion
    { _optionsQuestionUuid = question2 ^. uuid
    , _optionsQuestionTitle = "EDITED: Second Question"
    , _optionsQuestionText = Just "EDITED: Some long description"
    , _optionsQuestionRequiredPhaseUuid = Just $ phase3 ^. uuid
    , _optionsQuestionAnnotations = []
    , _optionsQuestionTagUuids = [tagDataScience ^. uuid, tagBioInformatic ^. uuid]
    , _optionsQuestionReferenceUuids = [km1_ch1_q2_r2 ^. uuid, km1_ch1_q2_r1 ^. uuid]
    , _optionsQuestionExpertUuids = [km1_ch1_q2_eNikola ^. uuid, km1_ch1_q2_eAlbert ^. uuid]
    , _optionsQuestionAnswerUuids = [q2_answerYes ^. uuid, q2_answerNo ^. uuid]
    }

question2WithNewType' :: Question
question2WithNewType' = ListQuestion' question2WithNewType

question2WithNewType :: ListQuestion
question2WithNewType =
  ListQuestion
    { _listQuestionUuid = question2 ^. uuid
    , _listQuestionTitle = "EDITED: " ++ question2 ^. title
    , _listQuestionText = question2 ^. text
    , _listQuestionRequiredPhaseUuid = question2 ^. requiredPhaseUuid
    , _listQuestionAnnotations = question2 ^. annotations
    , _listQuestionTagUuids = question2 ^. tagUuids
    , _listQuestionReferenceUuids = question2 ^. referenceUuids
    , _listQuestionExpertUuids = question2 ^. expertUuids
    , _listQuestionItemTemplateQuestionUuids = []
    }

question2Plain' :: Question
question2Plain' = OptionsQuestion' question2Plain

question2Plain :: OptionsQuestion
question2Plain =
  OptionsQuestion
    { _optionsQuestionUuid = question2 ^. uuid
    , _optionsQuestionTitle = question2 ^. title
    , _optionsQuestionText = question2 ^. text
    , _optionsQuestionRequiredPhaseUuid = question2 ^. requiredPhaseUuid
    , _optionsQuestionAnnotations = question2 ^. annotations
    , _optionsQuestionTagUuids = question2 ^. tagUuids
    , _optionsQuestionReferenceUuids = []
    , _optionsQuestionExpertUuids = []
    , _optionsQuestionAnswerUuids = []
    }

question3' :: Question
question3' = OptionsQuestion' question3

question3 :: OptionsQuestion
question3 =
  OptionsQuestion
    { _optionsQuestionUuid = u' "00000000-0000-0000-0000-0000000000f3"
    , _optionsQuestionTitle = "Third Question"
    , _optionsQuestionText = Just "Some long description"
    , _optionsQuestionRequiredPhaseUuid = Just $ phase2 ^. uuid
    , _optionsQuestionAnnotations = []
    , _optionsQuestionTagUuids = []
    , _optionsQuestionReferenceUuids = []
    , _optionsQuestionExpertUuids = []
    , _optionsQuestionAnswerUuids = [q3_answerNo ^. uuid, q3_answerYes ^. uuid]
    }

question3Plain' :: Question
question3Plain' = OptionsQuestion' question3Plain

question3Plain :: OptionsQuestion
question3Plain =
  OptionsQuestion
    { _optionsQuestionUuid = question3 ^. uuid
    , _optionsQuestionTitle = "Third Question"
    , _optionsQuestionText = Just "Some long description"
    , _optionsQuestionRequiredPhaseUuid = Just $ phase2 ^. uuid
    , _optionsQuestionAnnotations = []
    , _optionsQuestionTagUuids = []
    , _optionsQuestionReferenceUuids = []
    , _optionsQuestionExpertUuids = []
    , _optionsQuestionAnswerUuids = []
    }

question4' :: Question
question4' = ListQuestion' question4

question4 :: ListQuestion
question4 =
  ListQuestion
    { _listQuestionUuid = u' "00000000-0000-0000-0000-0000000000f4"
    , _listQuestionTitle = "Fourth Question"
    , _listQuestionText = Just "Some nice description"
    , _listQuestionRequiredPhaseUuid = Nothing
    , _listQuestionAnnotations = []
    , _listQuestionTagUuids = [tagBioInformatic ^. uuid]
    , _listQuestionReferenceUuids = []
    , _listQuestionExpertUuids = []
    , _listQuestionItemTemplateQuestionUuids = [q4_it1_question5 ^. uuid, q4_it1_question6 ^. uuid]
    }

question4Edited' :: Question
question4Edited' = ListQuestion' question4Edited

question4Edited :: ListQuestion
question4Edited =
  ListQuestion
    { _listQuestionUuid = question4 ^. uuid
    , _listQuestionTitle = "EDITED: " ++ question4 ^. title
    , _listQuestionText = Just $ "EDITED: " ++ fromJust (question4 ^. text)
    , _listQuestionRequiredPhaseUuid = Just $ phase2 ^. uuid
    , _listQuestionAnnotations = question4 ^. annotations
    , _listQuestionTagUuids = question4 ^. tagUuids
    , _listQuestionReferenceUuids = question4 ^. referenceUuids
    , _listQuestionExpertUuids = question4 ^. expertUuids
    , _listQuestionItemTemplateQuestionUuids = [q4_it1_question6 ^. uuid, q4_it1_question5 ^. uuid]
    }

question4WithNewType' :: Question
question4WithNewType' = IntegrationQuestion' question4WithNewType

question4WithNewType :: IntegrationQuestion
question4WithNewType =
  IntegrationQuestion
    { _integrationQuestionUuid = question4 ^. uuid
    , _integrationQuestionTitle = question4 ^. title
    , _integrationQuestionText = question4 ^. text
    , _integrationQuestionRequiredPhaseUuid = question4 ^. requiredPhaseUuid
    , _integrationQuestionAnnotations = question4 ^. annotations
    , _integrationQuestionTagUuids = question4 ^. tagUuids
    , _integrationQuestionReferenceUuids = question4 ^. referenceUuids
    , _integrationQuestionExpertUuids = question4 ^. expertUuids
    , _integrationQuestionIntegrationUuid = ontologyPortal ^. uuid
    , _integrationQuestionProps = Map.fromList [("domain", "biology"), ("country", "nl")]
    }

question4Plain' :: Question
question4Plain' = ListQuestion' question4Plain

question4Plain :: ListQuestion
question4Plain =
  ListQuestion
    { _listQuestionUuid = question4 ^. uuid
    , _listQuestionTitle = question4 ^. title
    , _listQuestionText = question4 ^. text
    , _listQuestionRequiredPhaseUuid = question4 ^. requiredPhaseUuid
    , _listQuestionAnnotations = question4 ^. annotations
    , _listQuestionTagUuids = question4 ^. tagUuids
    , _listQuestionReferenceUuids = question4 ^. referenceUuids
    , _listQuestionExpertUuids = question4 ^. expertUuids
    , _listQuestionItemTemplateQuestionUuids = []
    }

q4_it1_question5' :: Question
q4_it1_question5' = ListQuestion' q4_it1_question5

q4_it1_question5 :: ListQuestion
q4_it1_question5 =
  ListQuestion
    { _listQuestionUuid = u' "00000000-0000-0000-0000-0000000000f5"
    , _listQuestionTitle = "Fifth Question"
    , _listQuestionText = Just "Some funny description"
    , _listQuestionRequiredPhaseUuid = Just $ phase1 ^. uuid
    , _listQuestionAnnotations = []
    , _listQuestionTagUuids = [tagBioInformatic ^. uuid]
    , _listQuestionReferenceUuids = []
    , _listQuestionExpertUuids = []
    , _listQuestionItemTemplateQuestionUuids = [q4_it1_q5_it2_question7 ^. uuid, q4_it1_q5_it2_question8 ^. uuid]
    }

q4_it1_question5Plain' :: Question
q4_it1_question5Plain' = ListQuestion' q4_it1_question5Plain

q4_it1_question5Plain :: ListQuestion
q4_it1_question5Plain =
  ListQuestion
    { _listQuestionUuid = q4_it1_question5 ^. uuid
    , _listQuestionTitle = q4_it1_question5 ^. title
    , _listQuestionText = q4_it1_question5 ^. text
    , _listQuestionRequiredPhaseUuid = q4_it1_question5 ^. requiredPhaseUuid
    , _listQuestionAnnotations = q4_it1_question5 ^. annotations
    , _listQuestionTagUuids = q4_it1_question5 ^. tagUuids
    , _listQuestionReferenceUuids = q4_it1_question5 ^. referenceUuids
    , _listQuestionExpertUuids = q4_it1_question5 ^. expertUuids
    , _listQuestionItemTemplateQuestionUuids = []
    }

q4_it1_question5Edited' :: Question
q4_it1_question5Edited' = ListQuestion' q4_it1_question5Edited

q4_it1_question5Edited :: ListQuestion
q4_it1_question5Edited =
  ListQuestion
    { _listQuestionUuid = q4_it1_question5 ^. uuid
    , _listQuestionTitle = "EDITED: Fifth Question"
    , _listQuestionText = Just "EDITED: Some funny description"
    , _listQuestionRequiredPhaseUuid = Just $ phase3 ^. uuid
    , _listQuestionAnnotations = q4_it1_question5 ^. annotations
    , _listQuestionTagUuids = q4_it1_question5 ^. tagUuids
    , _listQuestionReferenceUuids = q4_it1_question5 ^. referenceUuids
    , _listQuestionExpertUuids = q4_it1_question5 ^. expertUuids
    , _listQuestionItemTemplateQuestionUuids = [q4_it1_q5_it2_question8 ^. uuid, q4_it1_q5_it2_question7 ^. uuid]
    }

q4_it1_question6' :: Question
q4_it1_question6' = OptionsQuestion' q4_it1_question6

q4_it1_question6 :: OptionsQuestion
q4_it1_question6 =
  OptionsQuestion
    { _optionsQuestionUuid = u' "00000000-0000-0000-0000-0000000000f6"
    , _optionsQuestionTitle = "Sixth Question"
    , _optionsQuestionText = Just "Some non-funny description"
    , _optionsQuestionRequiredPhaseUuid = Just $ phase2 ^. uuid
    , _optionsQuestionAnnotations = []
    , _optionsQuestionTagUuids = []
    , _optionsQuestionReferenceUuids = [km1_ch2_q6_r1 ^. uuid, km1_ch2_q6_r2 ^. uuid]
    , _optionsQuestionExpertUuids = [km1_ch2_q6_eAlbert ^. uuid, km1_ch2_q6_eNikola ^. uuid]
    , _optionsQuestionAnswerUuids = [q4_it1_q6_answerNo ^. uuid, q4_it1_q6_answerYes ^. uuid]
    }

q4_it1_question6Edited' :: Question
q4_it1_question6Edited' = OptionsQuestion' q4_it1_question6Edited

q4_it1_question6Edited :: OptionsQuestion
q4_it1_question6Edited =
  OptionsQuestion
    { _optionsQuestionUuid = q4_it1_question6 ^. uuid
    , _optionsQuestionTitle = "Sixth Question"
    , _optionsQuestionText = Just "Some non-funny description"
    , _optionsQuestionRequiredPhaseUuid = Nothing
    , _optionsQuestionAnnotations = []
    , _optionsQuestionTagUuids = []
    , _optionsQuestionReferenceUuids = [km1_ch2_q6_r2 ^. uuid, km1_ch2_q6_r1 ^. uuid]
    , _optionsQuestionExpertUuids = [km1_ch2_q6_eNikola ^. uuid, km1_ch2_q6_eAlbert ^. uuid]
    , _optionsQuestionAnswerUuids = [q4_it1_q6_answerYes ^. uuid, q4_it1_q6_answerNo ^. uuid]
    }

q4_it1_q5_it2_question7' :: Question
q4_it1_q5_it2_question7' = ValueQuestion' q4_it1_q5_it2_question7

q4_it1_q5_it2_question7 :: ValueQuestion
q4_it1_q5_it2_question7 =
  ValueQuestion
    { _valueQuestionUuid = u' "00000000-0000-0000-0000-0000000000f7"
    , _valueQuestionTitle = "Seventh Question"
    , _valueQuestionText = Just "Some non-funny description"
    , _valueQuestionRequiredPhaseUuid = Just $ phase2 ^. uuid
    , _valueQuestionAnnotations = []
    , _valueQuestionTagUuids = []
    , _valueQuestionReferenceUuids = []
    , _valueQuestionExpertUuids = []
    , _valueQuestionValueType = StringQuestionValueType
    }

q4_it1_q5_it2_question8' :: Question
q4_it1_q5_it2_question8' = ValueQuestion' q4_it1_q5_it2_question8

q4_it1_q5_it2_question8 :: ValueQuestion
q4_it1_q5_it2_question8 =
  ValueQuestion
    { _valueQuestionUuid = u' "00000000-0000-0000-0000-0000000000f8"
    , _valueQuestionTitle = "Eighth Question"
    , _valueQuestionText = Just "Some non-funny description"
    , _valueQuestionRequiredPhaseUuid = Just $ phase2 ^. uuid
    , _valueQuestionAnnotations = []
    , _valueQuestionTagUuids = []
    , _valueQuestionReferenceUuids = []
    , _valueQuestionExpertUuids = []
    , _valueQuestionValueType = StringQuestionValueType
    }

question9' :: Question
question9' = IntegrationQuestion' question9

question9 :: IntegrationQuestion
question9 =
  IntegrationQuestion
    { _integrationQuestionUuid = u' "00000000-0000-0000-0000-0000000000f9"
    , _integrationQuestionTitle = "Ninth Question"
    , _integrationQuestionText = Just "Some nice description"
    , _integrationQuestionRequiredPhaseUuid = Just $ phase1 ^. uuid
    , _integrationQuestionAnnotations = []
    , _integrationQuestionTagUuids = [tagBioInformatic ^. uuid]
    , _integrationQuestionReferenceUuids = []
    , _integrationQuestionExpertUuids = []
    , _integrationQuestionIntegrationUuid = ontologyPortal ^. uuid
    , _integrationQuestionProps = Map.fromList [("domain", "biology"), ("country", "nl")]
    }

question9Edited' :: Question
question9Edited' = IntegrationQuestion' question9Edited

question9Edited :: IntegrationQuestion
question9Edited =
  IntegrationQuestion
    { _integrationQuestionUuid = question9 ^. uuid
    , _integrationQuestionTitle = "EDITED: " ++ (question9 ^. title)
    , _integrationQuestionText = Just $ "EDITED: " ++ fromJust (question9 ^. text)
    , _integrationQuestionRequiredPhaseUuid = Just $ phase4 ^. uuid
    , _integrationQuestionAnnotations = []
    , _integrationQuestionTagUuids = [tagDataScience ^. uuid]
    , _integrationQuestionReferenceUuids = question9 ^. referenceUuids
    , _integrationQuestionExpertUuids = question9 ^. expertUuids
    , _integrationQuestionIntegrationUuid = question9 ^. integrationUuid
    , _integrationQuestionProps = Map.fromList [("domain", "biology"), ("country", "de")]
    }

question9PropsEdited' :: Question
question9PropsEdited' = IntegrationQuestion' question9PropsEdited

question9PropsEdited :: IntegrationQuestion
question9PropsEdited =
  IntegrationQuestion
    { _integrationQuestionUuid = question9 ^. uuid
    , _integrationQuestionTitle = question9 ^. title
    , _integrationQuestionText = question9 ^. text
    , _integrationQuestionRequiredPhaseUuid = question9 ^. requiredPhaseUuid
    , _integrationQuestionAnnotations = question9 ^. annotations
    , _integrationQuestionTagUuids = question9 ^. tagUuids
    , _integrationQuestionReferenceUuids = question9 ^. referenceUuids
    , _integrationQuestionExpertUuids = question9 ^. expertUuids
    , _integrationQuestionIntegrationUuid = question9 ^. integrationUuid
    , _integrationQuestionProps = Map.fromList [("domain", "biology"), ("language", "")]
    }

question9WithNewType' :: Question
question9WithNewType' = ValueQuestion' question9WithNewType

question9WithNewType :: ValueQuestion
question9WithNewType =
  ValueQuestion
    { _valueQuestionUuid = question9 ^. uuid
    , _valueQuestionTitle = "EDITED: " ++ question9 ^. title
    , _valueQuestionText = question9 ^. text
    , _valueQuestionRequiredPhaseUuid = question9 ^. requiredPhaseUuid
    , _valueQuestionAnnotations = question9 ^. annotations
    , _valueQuestionTagUuids = question9 ^. tagUuids
    , _valueQuestionReferenceUuids = question9 ^. referenceUuids
    , _valueQuestionExpertUuids = question9 ^. expertUuids
    , _valueQuestionValueType = DateQuestionValueType
    }

question9ConvertedToValue' :: Question
question9ConvertedToValue' = ValueQuestion' question9ConvertedToValue

question9ConvertedToValue :: ValueQuestion
question9ConvertedToValue =
  ValueQuestion
    { _valueQuestionUuid = question9 ^. uuid
    , _valueQuestionTitle = question9 ^. title
    , _valueQuestionText = question9 ^. text
    , _valueQuestionRequiredPhaseUuid = question9 ^. requiredPhaseUuid
    , _valueQuestionAnnotations = question9 ^. annotations
    , _valueQuestionTagUuids = question9 ^. tagUuids
    , _valueQuestionReferenceUuids = question9 ^. referenceUuids
    , _valueQuestionExpertUuids = question9 ^. expertUuids
    , _valueQuestionValueType = StringQuestionValueType
    }

question10' :: Question
question10' = IntegrationQuestion' question10

question10 :: IntegrationQuestion
question10 =
  IntegrationQuestion
    { _integrationQuestionUuid = u' "00000000-0000-0000-0000-000000000f10"
    , _integrationQuestionTitle = "Tenth Question"
    , _integrationQuestionText = Just "Some nice description"
    , _integrationQuestionRequiredPhaseUuid = Nothing
    , _integrationQuestionAnnotations = []
    , _integrationQuestionTagUuids = [tagBioInformatic ^. uuid]
    , _integrationQuestionReferenceUuids = []
    , _integrationQuestionExpertUuids = []
    , _integrationQuestionIntegrationUuid = bioPortal ^. uuid
    , _integrationQuestionProps = Map.fromList [("domain", "legal"), ("branch", "mammal")]
    }

question10ConvertedToValue' :: Question
question10ConvertedToValue' = ValueQuestion' question10ConvertedToValue

question10ConvertedToValue :: ValueQuestion
question10ConvertedToValue =
  ValueQuestion
    { _valueQuestionUuid = question10 ^. uuid
    , _valueQuestionTitle = question10 ^. title
    , _valueQuestionText = question10 ^. text
    , _valueQuestionRequiredPhaseUuid = question10 ^. requiredPhaseUuid
    , _valueQuestionAnnotations = question10 ^. annotations
    , _valueQuestionTagUuids = question10 ^. tagUuids
    , _valueQuestionReferenceUuids = question10 ^. referenceUuids
    , _valueQuestionExpertUuids = question10 ^. expertUuids
    , _valueQuestionValueType = StringQuestionValueType
    }

question11' :: Question
question11' = MultiChoiceQuestion' question11

question11 :: MultiChoiceQuestion
question11 =
  MultiChoiceQuestion
    { _multiChoiceQuestionUuid = u' "00000000-0000-0000-0000-000000000f11"
    , _multiChoiceQuestionTitle = "Eleventh Question"
    , _multiChoiceQuestionText = Just "Some non-funny description"
    , _multiChoiceQuestionRequiredPhaseUuid = Nothing
    , _multiChoiceQuestionAnnotations = []
    , _multiChoiceQuestionTagUuids = [tagBioInformatic ^. uuid]
    , _multiChoiceQuestionReferenceUuids = []
    , _multiChoiceQuestionExpertUuids = []
    , _multiChoiceQuestionChoiceUuids = [q11_choice1 ^. uuid, q11_choice2 ^. uuid]
    }

question11Edited' :: Question
question11Edited' = MultiChoiceQuestion' question11Edited

question11Edited :: MultiChoiceQuestion
question11Edited =
  question11
    { _multiChoiceQuestionTitle = "EDITED: Eleventh Question"
    , _multiChoiceQuestionText = Just "EDITED: Some non-funny description"
    , _multiChoiceQuestionRequiredPhaseUuid = Just $ phase2 ^. uuid
    , _multiChoiceQuestionAnnotations = []
    , _multiChoiceQuestionReferenceUuids = []
    , _multiChoiceQuestionExpertUuids = []
    , _multiChoiceQuestionChoiceUuids = [q11_choice2 ^. uuid, q11_choice1 ^. uuid]
    }

question12' :: Question
question12' = MultiChoiceQuestion' question12

question12 :: MultiChoiceQuestion
question12 =
  MultiChoiceQuestion
    { _multiChoiceQuestionUuid = u' "00000000-0000-0000-0000-000000000f12"
    , _multiChoiceQuestionTitle = "Twelfth Question"
    , _multiChoiceQuestionText = Just "Some non-funny description"
    , _multiChoiceQuestionRequiredPhaseUuid = Nothing
    , _multiChoiceQuestionAnnotations = []
    , _multiChoiceQuestionTagUuids = [tagBioInformatic ^. uuid]
    , _multiChoiceQuestionReferenceUuids = []
    , _multiChoiceQuestionExpertUuids = []
    , _multiChoiceQuestionChoiceUuids = []
    }
