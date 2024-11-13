module WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions where

import qualified Data.Map.Strict as Map

import Shared.Common.Util.Uuid
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations as FI
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.MetricMeasures
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases as PHS
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags as FT
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

-- -----------------------------------------------------------------
-- ANSWERS
-- -----------------------------------------------------------------
q2_answerNo :: Answer
q2_answerNo =
  Answer
    { uuid = u' "00000000-0000-0000-0000-00000000f2a1"
    , aLabel = "No"
    , advice = Just "Super long advice"
    , annotations = []
    , followUpUuids = []
    , metricMeasures = [metricMeasureF1, metricMeasureA1]
    }

q3_answerNo :: Answer
q3_answerNo =
  Answer
    { uuid = u' "00000000-0000-0000-0000-00000000f3a1"
    , aLabel = "No"
    , advice = Just "Super long advice"
    , annotations = []
    , followUpUuids = []
    , metricMeasures = [metricMeasureF1, metricMeasureA1]
    }

q4_it1_q6_answerNo :: Answer
q4_it1_q6_answerNo =
  Answer
    { uuid = u' "00000000-0000-0000-0000-00000000f6a1"
    , aLabel = "No"
    , advice = Just "Great advice"
    , annotations = []
    , followUpUuids = []
    , metricMeasures = []
    }

q2_aYes_fuq1_answerNo :: Answer
q2_aYes_fuq1_answerNo =
  Answer
    { uuid = u' "00000000-0000-0000-0000-a14d3387abc0"
    , aLabel = "No"
    , advice = Just "Super long advice"
    , annotations = []
    , followUpUuids = []
    , metricMeasures = []
    }

q2_aYes_fuq1_aYes_fuq2_answerNo :: Answer
q2_aYes_fuq1_aYes_fuq2_answerNo =
  Answer
    { uuid = u' "00000000-0000-0000-0000-03bb26fa2764"
    , aLabel = "No"
    , advice = Just "Super long advice"
    , annotations = []
    , followUpUuids = []
    , metricMeasures = [metricMeasureI0_5]
    }

q2_answerYes :: Answer
q2_answerYes =
  Answer
    { uuid = u' "00000000-0000-0000-0000-00000000f2a2"
    , aLabel = "Yes"
    , advice =
        Just
          "You know that this is very unlikely? This question is not only about data sets that are similar to what you want to determine yourself, but also reference data or data that should be mined from the existing literature. Further, it is very likely that you will refer to related data, e.g. other databases where you usually \"quickly look something up\", but that could maybe be properly integrated, especially if you need to do such lookups multiple times."
    , annotations = []
    , followUpUuids = [q2_aYes_fuQuestion1.uuid]
    , metricMeasures = [metricMeasureI1, metricMeasureR1]
    }

q2_answerYesEdited :: Answer
q2_answerYesEdited =
  Answer
    { uuid = q2_answerYes.uuid
    , aLabel = "EDITED: Yes"
    , advice = Just "EDITED: Short advice"
    , annotations = []
    , followUpUuids = []
    , metricMeasures = [metricMeasureI1, metricMeasureR1, metricMeasureG1]
    }

q2_answerYesPlain :: Answer
q2_answerYesPlain =
  Answer
    { uuid = u' "00000000-0000-0000-0000-00000000f2a2"
    , aLabel = "Yes"
    , advice =
        Just
          "You know that this is very unlikely? This question is not only about data sets that are similar to what you want to determine yourself, but also reference data or data that should be mined from the existing literature. Further, it is very likely that you will refer to related data, e.g. other databases where you usually \"quickly look something up\", but that could maybe be properly integrated, especially if you need to do such lookups multiple times."
    , annotations = []
    , followUpUuids = []
    , metricMeasures = [metricMeasureI1, metricMeasureR1]
    }

q3_answerYes :: Answer
q3_answerYes =
  Answer
    { uuid = u' "00000000-0000-0000-0000-00000000f3a2"
    , aLabel = "Yes"
    , advice = Just "Short advice"
    , annotations = []
    , followUpUuids = []
    , metricMeasures = []
    }

q2_aYes_fuq1_answerYes :: Answer
q2_aYes_fuq1_answerYes =
  Answer
    { uuid = u' "00000000-0000-0000-0000-8c80e6d66dcd"
    , aLabel = "Yes"
    , advice = Just "Short advice"
    , annotations = []
    , followUpUuids = [q2_aYes_fuq1_aYes_fuQuestion2.uuid]
    , metricMeasures = []
    }

q2_aYes_fuq1_aYes_fuq2_answerYes :: Answer
q2_aYes_fuq1_aYes_fuq2_answerYes =
  Answer
    { uuid = u' "00000000-0000-0000-0000-9f9aa5683efe"
    , aLabel = "Yes"
    , advice = Just "Short advice"
    , annotations = []
    , followUpUuids = []
    , metricMeasures = []
    }

q4_it1_q6_answerYes :: Answer
q4_it1_q6_answerYes =
  Answer
    { uuid = u' "00000000-0000-0000-0000-157e8c3cd009"
    , aLabel = "Yes"
    , advice = Just "Short advice"
    , annotations = []
    , followUpUuids = [q4_it1_q6_aYes_followUpQuestion4.uuid, q4_it1_q6_aYes_followUpQuestion5.uuid]
    , metricMeasures = []
    }

q2_answerMaybe :: Answer
q2_answerMaybe =
  Answer
    { uuid = u' "00000000-0000-0000-0000-00000000f2a3"
    , aLabel = "Maybe"
    , advice = Just "Great advice"
    , annotations = []
    , followUpUuids = []
    , metricMeasures = []
    }

-- -----------------------------------------------------------------
-- FOLLOW-UP QUESTIONS
-- -----------------------------------------------------------------
q2_aYes_fuQuestion1' :: Question
q2_aYes_fuQuestion1' = OptionsQuestion' q2_aYes_fuQuestion1

q2_aYes_fuQuestion1 :: OptionsQuestion
q2_aYes_fuQuestion1 =
  OptionsQuestion
    { uuid = u' "f9b380eb-bc18-4445-a9bf-14d9a1512d3f"
    , title = "First Follow-Up Question"
    , text = Just "Maybe there will be some description"
    , requiredPhaseUuid = Just $ PHS.phase2.uuid
    , annotations = []
    , tagUuids = [FT.tagDataScience.uuid]
    , referenceUuids = []
    , expertUuids = []
    , answerUuids = [q2_aYes_fuq1_answerNo.uuid, q2_aYes_fuq1_answerYes.uuid]
    }

q2_aYes_fuQuestion1Plain' :: Question
q2_aYes_fuQuestion1Plain' = OptionsQuestion' q2_aYes_fuQuestion1Plain

q2_aYes_fuQuestion1Plain :: OptionsQuestion
q2_aYes_fuQuestion1Plain =
  OptionsQuestion
    { uuid = q2_aYes_fuQuestion1.uuid
    , title = "Fourth Question"
    , text = Just "Just follow"
    , requiredPhaseUuid = Just $ PHS.phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , answerUuids = []
    }

-- -----------------------------------------------------------------------------
q2_aYes_fuq1_aYes_fuQuestion2' :: Question
q2_aYes_fuq1_aYes_fuQuestion2' = OptionsQuestion' q2_aYes_fuq1_aYes_fuQuestion2

q2_aYes_fuq1_aYes_fuQuestion2 :: OptionsQuestion
q2_aYes_fuq1_aYes_fuQuestion2 =
  OptionsQuestion
    { uuid = u' "00000000-27bd-4156-9b2d-c4e8c582cca8"
    , title = "Second Follow-Up Question"
    , text = Just "Again just follow"
    , requiredPhaseUuid = Just $ PHS.phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , answerUuids = [q2_aYes_fuq1_aYes_fuq2_answerNo.uuid, q2_aYes_fuq1_aYes_fuq2_answerYes.uuid]
    }

q2_aYes_fuq1_aYes_fuQuestion2Edited' :: Question
q2_aYes_fuq1_aYes_fuQuestion2Edited' = OptionsQuestion' q2_aYes_fuq1_aYes_fuQuestion2Edited

q2_aYes_fuq1_aYes_fuQuestion2Edited :: OptionsQuestion
q2_aYes_fuq1_aYes_fuQuestion2Edited =
  OptionsQuestion
    { uuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , title = "EDITED: Second Follow-Up Question"
    , text = Just "EDITED: Again just follow"
    , requiredPhaseUuid = Just $ PHS.phase1.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , answerUuids = [q2_aYes_fuq1_aYes_fuq2_answerYes.uuid, q2_aYes_fuq1_aYes_fuq2_answerNo.uuid]
    }

-- -----------------------------------------------------------------------------
q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3' :: Question
q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3' = OptionsQuestion' q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3

q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 :: OptionsQuestion
q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 =
  OptionsQuestion
    { uuid = u' "00000000-bd35-4d5e-8995-78a94a69da83"
    , title = "Third Follow-Up Question"
    , text = Just "Again and again just follow"
    , requiredPhaseUuid = Just $ PHS.phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , answerUuids = []
    }

-- -----------------------------------------------------------------------------
q4_it1_q6_aYes_followUpQuestion4' :: Question
q4_it1_q6_aYes_followUpQuestion4' = ListQuestion' q4_it1_q6_aYes_followUpQuestion4

q4_it1_q6_aYes_followUpQuestion4 :: ListQuestion
q4_it1_q6_aYes_followUpQuestion4 =
  ListQuestion
    { uuid = u' "cd98f76a-a430-4bd6-ba63-eb4c3c5c8c7e"
    , title = "Fourth Follow-Up Question"
    , text = Just "Again and again just follow"
    , requiredPhaseUuid = Just $ PHS.phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , itemTemplateQuestionUuids =
        [q4_it1_q6_aYes_fuq4_it_question1.uuid, q4_it1_q6_aYes_fuq4_it_question2.uuid]
    }

q4_it1_q6_aYes_followUpQuestion4Edited' :: Question
q4_it1_q6_aYes_followUpQuestion4Edited' = ListQuestion' q4_it1_q6_aYes_followUpQuestion4Edited

q4_it1_q6_aYes_followUpQuestion4Edited :: ListQuestion
q4_it1_q6_aYes_followUpQuestion4Edited =
  ListQuestion
    { uuid = q4_it1_q6_aYes_followUpQuestion4.uuid
    , title = "EDITED: Third Follow-Up Question"
    , text = Just "EDITED: Again and again just follow"
    , requiredPhaseUuid = Just $ PHS.phase1.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , itemTemplateQuestionUuids =
        [q4_it1_q6_aYes_fuq4_it_question2.uuid, q4_it1_q6_aYes_fuq4_it_question1.uuid]
    }

q4_it1_q6_aYes_fuq4_it_question1' :: Question
q4_it1_q6_aYes_fuq4_it_question1' = OptionsQuestion' q4_it1_q6_aYes_fuq4_it_question1

q4_it1_q6_aYes_fuq4_it_question1 :: OptionsQuestion
q4_it1_q6_aYes_fuq4_it_question1 =
  OptionsQuestion
    { uuid = u' "e5a3e1b2-077a-405f-b35c-3bffded63140"
    , title = "Sub question 1 of Follow-Up Question 4"
    , text = Just "Again and again just follow"
    , requiredPhaseUuid = Just $ PHS.phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , answerUuids = []
    }

q4_it1_q6_aYes_fuq4_it_question2' :: Question
q4_it1_q6_aYes_fuq4_it_question2' = OptionsQuestion' q4_it1_q6_aYes_fuq4_it_question2

q4_it1_q6_aYes_fuq4_it_question2 :: OptionsQuestion
q4_it1_q6_aYes_fuq4_it_question2 =
  OptionsQuestion
    { uuid = u' "7f2e3fe5-b8b6-4b5a-812d-c5c1c704b3d9"
    , title = "Sub question 2 of Follow-Up Question 4"
    , text = Just "Again and again just follow"
    , requiredPhaseUuid = Just $ PHS.phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , answerUuids = []
    }

q4_it1_q6_aYes_followUpQuestion5' :: Question
q4_it1_q6_aYes_followUpQuestion5' = IntegrationQuestion' q4_it1_q6_aYes_followUpQuestion5

q4_it1_q6_aYes_followUpQuestion5 :: IntegrationQuestion
q4_it1_q6_aYes_followUpQuestion5 =
  IntegrationQuestion
    { uuid = u' "82f9a83a-88c8-439b-8cf8-8a028d5cce7d"
    , title = "Fifth Follow-Up Question"
    , text = Just "Some non-funny description"
    , requiredPhaseUuid = Just $ PHS.phase2.uuid
    , annotations = []
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , integrationUuid = FI.ontologyPortal.uuid
    , props = Map.fromList [("domain", "biology"), ("country", "be")]
    }

q4_it1_q6_aYes_fuq5PropsEdited' :: Question
q4_it1_q6_aYes_fuq5PropsEdited' = IntegrationQuestion' q4_it1_q6_aYes_fuq5PropsEdited

q4_it1_q6_aYes_fuq5PropsEdited :: IntegrationQuestion
q4_it1_q6_aYes_fuq5PropsEdited =
  IntegrationQuestion
    { uuid = q4_it1_q6_aYes_followUpQuestion5.uuid
    , title = q4_it1_q6_aYes_followUpQuestion5.title
    , text = q4_it1_q6_aYes_followUpQuestion5.text
    , requiredPhaseUuid = q4_it1_q6_aYes_followUpQuestion5.requiredPhaseUuid
    , annotations = []
    , tagUuids = q4_it1_q6_aYes_followUpQuestion5.tagUuids
    , referenceUuids = q4_it1_q6_aYes_followUpQuestion5.referenceUuids
    , expertUuids = q4_it1_q6_aYes_followUpQuestion5.expertUuids
    , integrationUuid = q4_it1_q6_aYes_followUpQuestion5.integrationUuid
    , props = Map.fromList [("domain", "biology"), ("language", "")]
    }

q4_it1_q6_aYes_fuq5ConvertedToValue' :: Question
q4_it1_q6_aYes_fuq5ConvertedToValue' = ValueQuestion' q4_it1_q6_aYes_fuq5ConvertedToValue

q4_it1_q6_aYes_fuq5ConvertedToValue :: ValueQuestion
q4_it1_q6_aYes_fuq5ConvertedToValue =
  ValueQuestion
    { uuid = q4_it1_q6_aYes_followUpQuestion5.uuid
    , title = q4_it1_q6_aYes_followUpQuestion5.title
    , text = q4_it1_q6_aYes_followUpQuestion5.text
    , requiredPhaseUuid = q4_it1_q6_aYes_followUpQuestion5.requiredPhaseUuid
    , annotations = []
    , tagUuids = q4_it1_q6_aYes_followUpQuestion5.tagUuids
    , referenceUuids = q4_it1_q6_aYes_followUpQuestion5.referenceUuids
    , expertUuids = q4_it1_q6_aYes_followUpQuestion5.expertUuids
    , valueType = StringQuestionValueType
    , validations = []
    }
