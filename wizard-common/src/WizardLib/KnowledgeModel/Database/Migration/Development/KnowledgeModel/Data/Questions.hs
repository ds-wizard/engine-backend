module WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions where

import qualified Data.Map.Strict as Map
import Data.Maybe

import Shared.Common.Util.Uuid
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Choices
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Experts
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.References
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

question1' :: Question
question1' = ValueQuestion' question1

question1 :: ValueQuestion
question1 =
  ValueQuestion
    { uuid = u' "00000000-0000-0000-0000-0000000000f1"
    , title = "First Question"
    , text = Just "Here is a description of question"
    , requiredPhaseUuid = Just $ phase1.uuid
    , annotations = []
    , tagUuids = [tagDataScience.uuid, tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , valueType = StringQuestionValueType
    , validations = [OrcidQuestionValidation]
    }

question1Edited' :: Question
question1Edited' = ValueQuestion' question1Edited

question1Edited :: ValueQuestion
question1Edited =
  ValueQuestion
    { uuid = question1.uuid
    , title = "EDITED: " ++ question1.title
    , text = question1.text
    , requiredPhaseUuid = question1.requiredPhaseUuid
    , annotations = question1.annotations
    , tagUuids = question1.tagUuids
    , referenceUuids = question1.referenceUuids
    , expertUuids = question1.expertUuids
    , valueType = question1.valueType
    , validations = [OrcidQuestionValidation, DoiQuestionValidation]
    }

question1WithNewType' :: Question
question1WithNewType' = OptionsQuestion' question1WithNewType

question1WithNewType :: OptionsQuestion
question1WithNewType =
  OptionsQuestion
    { uuid = question1.uuid
    , title = "EDITED: " ++ question1.title
    , text = question1.text
    , requiredPhaseUuid = question1.requiredPhaseUuid
    , annotations = question1.annotations
    , tagUuids = question1.tagUuids
    , referenceUuids = question1.referenceUuids
    , expertUuids = question1.expertUuids
    , answerUuids = []
    }

-- -----------------------------------
question2' :: Question
question2' = OptionsQuestion' question2

question2 :: OptionsQuestion
question2 =
  OptionsQuestion
    { uuid = u' "00000000-0000-0000-0000-0000000000f2"
    , title = "Is there any pre-existing data?"
    , text =
        Just "Are there any data sets available in the world that are relevant to your planned research?"
    , requiredPhaseUuid = Just $ phase2.uuid
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = [km1_ch1_q2_r1.uuid, km1_ch1_q2_r2.uuid]
    , expertUuids = [km1_ch1_q2_eAlbert.uuid, km1_ch1_q2_eNikola.uuid]
    , answerUuids = [q2_answerNo.uuid, q2_answerYes.uuid]
    }

question2Edited' :: Question
question2Edited' = OptionsQuestion' question2Edited

question2Edited :: OptionsQuestion
question2Edited =
  OptionsQuestion
    { uuid = question2.uuid
    , title = "EDITED: Second Question"
    , text = Just "EDITED: Some long description"
    , requiredPhaseUuid = Just $ phase3.uuid
    , annotations = []
    , tagUuids = [tagDataScience.uuid, tagBioInformatic.uuid]
    , referenceUuids = [km1_ch1_q2_r2.uuid, km1_ch1_q2_r1.uuid]
    , expertUuids = [km1_ch1_q2_eNikola.uuid, km1_ch1_q2_eAlbert.uuid]
    , answerUuids = [q2_answerYes.uuid, q2_answerNo.uuid]
    }

question2WithNewType' :: Question
question2WithNewType' = ListQuestion' question2WithNewType

question2WithNewType :: ListQuestion
question2WithNewType =
  ListQuestion
    { uuid = question2.uuid
    , title = "EDITED: " ++ question2.title
    , text = question2.text
    , requiredPhaseUuid = question2.requiredPhaseUuid
    , annotations = question2.annotations
    , tagUuids = question2.tagUuids
    , referenceUuids = question2.referenceUuids
    , expertUuids = question2.expertUuids
    , itemTemplateQuestionUuids = []
    }

question2Plain' :: Question
question2Plain' = OptionsQuestion' question2Plain

question2Plain :: OptionsQuestion
question2Plain =
  OptionsQuestion
    { uuid = question2.uuid
    , title = question2.title
    , text = question2.text
    , requiredPhaseUuid = question2.requiredPhaseUuid
    , annotations = question2.annotations
    , tagUuids = question2.tagUuids
    , referenceUuids = []
    , expertUuids = []
    , answerUuids = []
    }

question3' :: Question
question3' = OptionsQuestion' question3

question3 :: OptionsQuestion
question3 =
  OptionsQuestion
    { uuid = u' "00000000-0000-0000-0000-0000000000f3"
    , title = "Third Question"
    , text = Just "Some long description"
    , requiredPhaseUuid = Just $ phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , answerUuids = [q3_answerNo.uuid, q3_answerYes.uuid]
    }

question3Plain' :: Question
question3Plain' = OptionsQuestion' question3Plain

question3Plain :: OptionsQuestion
question3Plain =
  OptionsQuestion
    { uuid = question3.uuid
    , title = "Third Question"
    , text = Just "Some long description"
    , requiredPhaseUuid = Just $ phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , answerUuids = []
    }

question4' :: Question
question4' = ListQuestion' question4

question4 :: ListQuestion
question4 =
  ListQuestion
    { uuid = u' "00000000-0000-0000-0000-0000000000f4"
    , title = "Fourth Question"
    , text = Just "Some nice description"
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , itemTemplateQuestionUuids = [q4_it1_question5.uuid, q4_it1_question6.uuid]
    }

question4Edited' :: Question
question4Edited' = ListQuestion' question4Edited

question4Edited :: ListQuestion
question4Edited =
  ListQuestion
    { uuid = question4.uuid
    , title = "EDITED: " ++ question4.title
    , text = Just $ "EDITED: " ++ fromJust question4.text
    , requiredPhaseUuid = Just $ phase2.uuid
    , annotations = question4.annotations
    , tagUuids = question4.tagUuids
    , referenceUuids = question4.referenceUuids
    , expertUuids = question4.expertUuids
    , itemTemplateQuestionUuids = [q4_it1_question6.uuid, q4_it1_question5.uuid]
    }

question4WithNewType' :: Question
question4WithNewType' = IntegrationQuestion' question4WithNewType

question4WithNewType :: IntegrationQuestion
question4WithNewType =
  IntegrationQuestion
    { uuid = question4.uuid
    , title = question4.title
    , text = question4.text
    , requiredPhaseUuid = question4.requiredPhaseUuid
    , annotations = question4.annotations
    , tagUuids = question4.tagUuids
    , referenceUuids = question4.referenceUuids
    , expertUuids = question4.expertUuids
    , integrationUuid = ontologyPortal.uuid
    , props = Map.fromList [("domain", "biology"), ("country", "nl")]
    }

question4Plain' :: Question
question4Plain' = ListQuestion' question4Plain

question4Plain :: ListQuestion
question4Plain =
  ListQuestion
    { uuid = question4.uuid
    , title = question4.title
    , text = question4.text
    , requiredPhaseUuid = question4.requiredPhaseUuid
    , annotations = question4.annotations
    , tagUuids = question4.tagUuids
    , referenceUuids = question4.referenceUuids
    , expertUuids = question4.expertUuids
    , itemTemplateQuestionUuids = []
    }

q4_it1_question5' :: Question
q4_it1_question5' = ListQuestion' q4_it1_question5

q4_it1_question5 :: ListQuestion
q4_it1_question5 =
  ListQuestion
    { uuid = u' "00000000-0000-0000-0000-0000000000f5"
    , title = "Fifth Question"
    , text = Just "Some funny description"
    , requiredPhaseUuid = Just $ phase1.uuid
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , itemTemplateQuestionUuids = [q4_it1_q5_it2_question7.uuid, q4_it1_q5_it2_question8.uuid]
    }

q4_it1_question5Plain' :: Question
q4_it1_question5Plain' = ListQuestion' q4_it1_question5Plain

q4_it1_question5Plain :: ListQuestion
q4_it1_question5Plain =
  ListQuestion
    { uuid = q4_it1_question5.uuid
    , title = q4_it1_question5.title
    , text = q4_it1_question5.text
    , requiredPhaseUuid = q4_it1_question5.requiredPhaseUuid
    , annotations = q4_it1_question5.annotations
    , tagUuids = q4_it1_question5.tagUuids
    , referenceUuids = q4_it1_question5.referenceUuids
    , expertUuids = q4_it1_question5.expertUuids
    , itemTemplateQuestionUuids = []
    }

q4_it1_question5Edited' :: Question
q4_it1_question5Edited' = ListQuestion' q4_it1_question5Edited

q4_it1_question5Edited :: ListQuestion
q4_it1_question5Edited =
  ListQuestion
    { uuid = q4_it1_question5.uuid
    , title = "EDITED: Fifth Question"
    , text = Just "EDITED: Some funny description"
    , requiredPhaseUuid = Just $ phase3.uuid
    , annotations = q4_it1_question5.annotations
    , tagUuids = q4_it1_question5.tagUuids
    , referenceUuids = q4_it1_question5.referenceUuids
    , expertUuids = q4_it1_question5.expertUuids
    , itemTemplateQuestionUuids = [q4_it1_q5_it2_question8.uuid, q4_it1_q5_it2_question7.uuid]
    }

q4_it1_question6' :: Question
q4_it1_question6' = OptionsQuestion' q4_it1_question6

q4_it1_question6 :: OptionsQuestion
q4_it1_question6 =
  OptionsQuestion
    { uuid = u' "00000000-0000-0000-0000-0000000000f6"
    , title = "Sixth Question"
    , text = Just "Some non-funny description"
    , requiredPhaseUuid = Just $ phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = [km1_ch2_q6_r1.uuid, km1_ch2_q6_r2.uuid]
    , expertUuids = [km1_ch2_q6_eAlbert.uuid, km1_ch2_q6_eNikola.uuid]
    , answerUuids = [q4_it1_q6_answerNo.uuid, q4_it1_q6_answerYes.uuid]
    }

q4_it1_question6Edited' :: Question
q4_it1_question6Edited' = OptionsQuestion' q4_it1_question6Edited

q4_it1_question6Edited :: OptionsQuestion
q4_it1_question6Edited =
  OptionsQuestion
    { uuid = q4_it1_question6.uuid
    , title = "Sixth Question"
    , text = Just "Some non-funny description"
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = []
    , referenceUuids = [km1_ch2_q6_r2.uuid, km1_ch2_q6_r1.uuid]
    , expertUuids = [km1_ch2_q6_eNikola.uuid, km1_ch2_q6_eAlbert.uuid]
    , answerUuids = [q4_it1_q6_answerYes.uuid, q4_it1_q6_answerNo.uuid]
    }

q4_it1_q5_it2_question7' :: Question
q4_it1_q5_it2_question7' = ValueQuestion' q4_it1_q5_it2_question7

q4_it1_q5_it2_question7 :: ValueQuestion
q4_it1_q5_it2_question7 =
  ValueQuestion
    { uuid = u' "00000000-0000-0000-0000-0000000000f7"
    , title = "Seventh Question"
    , text = Just "Some non-funny description"
    , requiredPhaseUuid = Just $ phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , valueType = StringQuestionValueType
    , validations = []
    }

q4_it1_q5_it2_question8' :: Question
q4_it1_q5_it2_question8' = ValueQuestion' q4_it1_q5_it2_question8

q4_it1_q5_it2_question8 :: ValueQuestion
q4_it1_q5_it2_question8 =
  ValueQuestion
    { uuid = u' "00000000-0000-0000-0000-0000000000f8"
    , title = "Eighth Question"
    , text = Just "Some non-funny description"
    , requiredPhaseUuid = Just $ phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , valueType = StringQuestionValueType
    , validations = []
    }

question9' :: Question
question9' = IntegrationQuestion' question9

question9 :: IntegrationQuestion
question9 =
  IntegrationQuestion
    { uuid = u' "00000000-0000-0000-0000-0000000000f9"
    , title = "Ninth Question"
    , text = Just "Some nice description"
    , requiredPhaseUuid = Just $ phase1.uuid
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , integrationUuid = ontologyPortal.uuid
    , props = Map.fromList [("domain", "biology"), ("country", "nl")]
    }

question9Edited' :: Question
question9Edited' = IntegrationQuestion' question9Edited

question9Edited :: IntegrationQuestion
question9Edited =
  IntegrationQuestion
    { uuid = question9.uuid
    , title = "EDITED: " ++ question9.title
    , text = Just $ "EDITED: " ++ fromJust question9.text
    , requiredPhaseUuid = Just $ phase4.uuid
    , annotations = []
    , tagUuids = [tagDataScience.uuid]
    , referenceUuids = question9.referenceUuids
    , expertUuids = question9.expertUuids
    , integrationUuid = question9.integrationUuid
    , props = Map.fromList [("domain", "biology"), ("country", "de")]
    }

question9PropsEdited' :: Question
question9PropsEdited' = IntegrationQuestion' question9PropsEdited

question9PropsEdited :: IntegrationQuestion
question9PropsEdited =
  IntegrationQuestion
    { uuid = question9.uuid
    , title = question9.title
    , text = question9.text
    , requiredPhaseUuid = question9.requiredPhaseUuid
    , annotations = question9.annotations
    , tagUuids = question9.tagUuids
    , referenceUuids = question9.referenceUuids
    , expertUuids = question9.expertUuids
    , integrationUuid = question9.integrationUuid
    , props = Map.fromList [("domain", "biology"), ("language", "")]
    }

question9WithNewType' :: Question
question9WithNewType' = ValueQuestion' question9WithNewType

question9WithNewType :: ValueQuestion
question9WithNewType =
  ValueQuestion
    { uuid = question9.uuid
    , title = "EDITED: " ++ question9.title
    , text = question9.text
    , requiredPhaseUuid = question9.requiredPhaseUuid
    , annotations = question9.annotations
    , tagUuids = question9.tagUuids
    , referenceUuids = question9.referenceUuids
    , expertUuids = question9.expertUuids
    , valueType = DateQuestionValueType
    , validations = []
    }

question9ConvertedToValue' :: Question
question9ConvertedToValue' = ValueQuestion' question9ConvertedToValue

question9ConvertedToValue :: ValueQuestion
question9ConvertedToValue =
  ValueQuestion
    { uuid = question9.uuid
    , title = question9.title
    , text = question9.text
    , requiredPhaseUuid = question9.requiredPhaseUuid
    , annotations = question9.annotations
    , tagUuids = question9.tagUuids
    , referenceUuids = question9.referenceUuids
    , expertUuids = question9.expertUuids
    , valueType = StringQuestionValueType
    , validations = []
    }

question10' :: Question
question10' = IntegrationQuestion' question10

question10 :: IntegrationQuestion
question10 =
  IntegrationQuestion
    { uuid = u' "00000000-0000-0000-0000-000000000f10"
    , title = "Tenth Question"
    , text = Just "Some nice description"
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , integrationUuid = bioPortal.uuid
    , props = Map.fromList [("domain", "legal"), ("branch", "mammal")]
    }

question10ConvertedToValue' :: Question
question10ConvertedToValue' = ValueQuestion' question10ConvertedToValue

question10ConvertedToValue :: ValueQuestion
question10ConvertedToValue =
  ValueQuestion
    { uuid = question10.uuid
    , title = question10.title
    , text = question10.text
    , requiredPhaseUuid = question10.requiredPhaseUuid
    , annotations = question10.annotations
    , tagUuids = question10.tagUuids
    , referenceUuids = question10.referenceUuids
    , expertUuids = question10.expertUuids
    , valueType = StringQuestionValueType
    , validations = []
    }

question11' :: Question
question11' = MultiChoiceQuestion' question11

question11 :: MultiChoiceQuestion
question11 =
  MultiChoiceQuestion
    { uuid = u' "00000000-0000-0000-0000-000000000f11"
    , title = "Eleventh Question"
    , text = Just "Some non-funny description"
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , choiceUuids = [q11_choice1.uuid, q11_choice2.uuid]
    }

question11Edited' :: Question
question11Edited' = MultiChoiceQuestion' question11Edited

question11Edited :: MultiChoiceQuestion
question11Edited =
  question11
    { title = "EDITED: Eleventh Question"
    , text = Just "EDITED: Some non-funny description"
    , requiredPhaseUuid = Just $ phase2.uuid
    , annotations = []
    , referenceUuids = []
    , expertUuids = []
    , choiceUuids = [q11_choice2.uuid, q11_choice1.uuid]
    }

question12' :: Question
question12' = MultiChoiceQuestion' question12

question12 :: MultiChoiceQuestion
question12 =
  MultiChoiceQuestion
    { uuid = u' "00000000-0000-0000-0000-000000000f12"
    , title = "Twelfth Question"
    , text = Just "Some non-funny description"
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , choiceUuids = []
    }

question13' :: Question
question13' = ItemSelectQuestion' question13

question13 :: ItemSelectQuestion
question13 =
  ItemSelectQuestion
    { uuid = u' "00000000-0000-0000-0000-000000000f13"
    , title = "Thirteen Question"
    , text = Just "Some non-funny description"
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , listQuestionUuid = Just question4.uuid
    }

question13Edited' :: Question
question13Edited' = ItemSelectQuestion' question13Edited

question13Edited :: ItemSelectQuestion
question13Edited =
  ItemSelectQuestion
    { uuid = question13.uuid
    , title = "EDITED: Thirteen Question"
    , text = Just "EDITED: Some non-funny description"
    , requiredPhaseUuid = Just $ phase2.uuid
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , listQuestionUuid = Just q4_it1_question5.uuid
    }

question14' :: Question
question14' = FileQuestion' question14

question14 :: FileQuestion
question14 =
  FileQuestion
    { uuid = u' "00000000-0000-0000-0000-000000000f14"
    , title = "Fourteen Question"
    , text = Just "Some non-funny description"
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , maxSize = Just 20000
    , fileTypes = Just "application/json"
    }

question14Edited' :: Question
question14Edited' = FileQuestion' question14Edited

question14Edited :: FileQuestion
question14Edited =
  FileQuestion
    { uuid = question14.uuid
    , title = "EDITED: Fourteen Question"
    , text = Just "EDITED: Some non-funny description"
    , requiredPhaseUuid = Just $ phase2.uuid
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , maxSize = Just 40000
    , fileTypes = Just "text/html"
    }

question15' :: Question
question15' = IntegrationQuestion' question15

question15 :: IntegrationQuestion
question15 =
  IntegrationQuestion
    { uuid = u' "00000000-0000-0000-0000-000000000f15"
    , title = "Fifteen Question"
    , text = Nothing
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = [tagBioInformatic.uuid]
    , referenceUuids = []
    , expertUuids = []
    , integrationUuid = repositoryApi.uuid
    , props = Map.fromList [("domain", "biology"), ("country", "nl")]
    }
