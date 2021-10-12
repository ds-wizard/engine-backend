module Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Map.Strict as M

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations as FI
import Shared.Database.Migration.Development.KnowledgeModel.Data.MetricMeasures
import Shared.Database.Migration.Development.KnowledgeModel.Data.Phases as PHS
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags as FT
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

-- -----------------------------------------------------------------
-- ANSWERS
-- -----------------------------------------------------------------
q2_answerNo :: Answer
q2_answerNo =
  Answer
    { _answerUuid = u' "33da0831-11dd-4faa-b754-41ed98dedcb5"
    , _answerLabel = "No"
    , _answerAdvice = Just "Super long advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = [metricMeasureF1, metricMeasureA1]
    }

q3_answerNo :: Answer
q3_answerNo =
  Answer
    { _answerUuid = u' "12711c8c-193a-4baf-a071-53f2d3990083"
    , _answerLabel = "No"
    , _answerAdvice = Just "Super long advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = [metricMeasureF1, metricMeasureA1]
    }

q4_it1_q6_answerNo :: Answer
q4_it1_q6_answerNo =
  Answer
    { _answerUuid = u' "a093c2c3-123c-42ee-9667-13af14b6249e"
    , _answerLabel = "No"
    , _answerAdvice = Just "Great advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = []
    }

q2_aYes_fuq1_answerNo :: Answer
q2_aYes_fuq1_answerNo =
  Answer
    { _answerUuid = u' "8ebf2494-80c7-4dbb-a4a1-a14d3387abc0"
    , _answerLabel = "No"
    , _answerAdvice = Just "Super long advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = []
    }

q2_aYes_fuq1_aYes_fuq2_answerNo :: Answer
q2_aYes_fuq1_aYes_fuq2_answerNo =
  Answer
    { _answerUuid = u' "891ebfe2-27df-433c-af83-03bb26fa2764"
    , _answerLabel = "No"
    , _answerAdvice = Just "Super long advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = [metricMeasureI0_5]
    }

q2_answerYes :: Answer
q2_answerYes =
  Answer
    { _answerUuid = u' "d6fb1eb3-3bef-4aac-8491-def68f40ac78"
    , _answerLabel = "Yes"
    , _answerAdvice =
        Just
          "You know that this is very unlikely? This question is not only about data sets that are similar to what you want to determine yourself, but also reference data or data that should be mined from the existing literature. Further, it is very likely that you will refer to related data, e.g. other databases where you usually \"quickly look something up\", but that could maybe be properly integrated, especially if you need to do such lookups multiple times."
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = [q2_aYes_fuQuestion1 ^. uuid]
    , _answerMetricMeasures = [metricMeasureI1, metricMeasureR1]
    }

q2_answerYesEdited :: Answer
q2_answerYesEdited =
  Answer
    { _answerUuid = q2_answerYes ^. uuid
    , _answerLabel = "EDITED: Yes"
    , _answerAdvice = Just "EDITED: Short advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = [metricMeasureI1, metricMeasureR1, metricMeasureG1]
    }

q2_answerYesPlain :: Answer
q2_answerYesPlain =
  Answer
    { _answerUuid = u' "d6fb1eb3-3bef-4aac-8491-def68f40ac78"
    , _answerLabel = "Yes"
    , _answerAdvice =
        Just
          "You know that this is very unlikely? This question is not only about data sets that are similar to what you want to determine yourself, but also reference data or data that should be mined from the existing literature. Further, it is very likely that you will refer to related data, e.g. other databases where you usually \"quickly look something up\", but that could maybe be properly integrated, especially if you need to do such lookups multiple times."
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = [metricMeasureI1, metricMeasureR1]
    }

q3_answerYes :: Answer
q3_answerYes =
  Answer
    { _answerUuid = u' "28d49dbe-4180-49c9-80b2-397e9ea27c77"
    , _answerLabel = "Yes"
    , _answerAdvice = Just "Short advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = []
    }

q2_aYes_fuq1_answerYes :: Answer
q2_aYes_fuq1_answerYes =
  Answer
    { _answerUuid = u' "4d164317-d900-460c-8582-8c80e6d66dcd"
    , _answerLabel = "Yes"
    , _answerAdvice = Just "Short advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = [q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid]
    , _answerMetricMeasures = []
    }

q2_aYes_fuq1_aYes_fuq2_answerYes :: Answer
q2_aYes_fuq1_aYes_fuq2_answerYes =
  Answer
    { _answerUuid = u' "b6b40918-a9b7-4d2d-bacb-9f9aa5683efe"
    , _answerLabel = "Yes"
    , _answerAdvice = Just "Short advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = []
    }

q4_it1_q6_answerYes :: Answer
q4_it1_q6_answerYes =
  Answer
    { _answerUuid = u' "16f20d73-b335-47d8-8d35-157e8c3cd009"
    , _answerLabel = "Yes"
    , _answerAdvice = Just "Short advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = [q4_it1_q6_aYes_followUpQuestion4 ^. uuid, q4_it1_q6_aYes_followUpQuestion5 ^. uuid]
    , _answerMetricMeasures = []
    }

q2_answerMaybe :: Answer
q2_answerMaybe =
  Answer
    { _answerUuid = u' "1f172f5e-3d66-4a1c-a785-85ba02fcf72a"
    , _answerLabel = "Maybe"
    , _answerAdvice = Just "Great advice"
    , _answerAnnotations = M.empty
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = []
    }

-- -----------------------------------------------------------------
-- FOLLOW-UP QUESTIONS
-- -----------------------------------------------------------------
q2_aYes_fuQuestion1' :: Question
q2_aYes_fuQuestion1' = OptionsQuestion' q2_aYes_fuQuestion1

q2_aYes_fuQuestion1 :: OptionsQuestion
q2_aYes_fuQuestion1 =
  OptionsQuestion
    { _optionsQuestionUuid = u' "f9b380eb-bc18-4445-a9bf-14d9a1512d3f"
    , _optionsQuestionTitle = "First Follow-Up Question"
    , _optionsQuestionText = Just "Maybe there will be some description"
    , _optionsQuestionRequiredPhaseUuid = Just $ PHS.phase2 ^. uuid
    , _optionsQuestionAnnotations = M.empty
    , _optionsQuestionTagUuids = [FT.tagDataScience ^. uuid]
    , _optionsQuestionReferenceUuids = []
    , _optionsQuestionExpertUuids = []
    , _optionsQuestionAnswerUuids = [q2_aYes_fuq1_answerNo ^. uuid, q2_aYes_fuq1_answerYes ^. uuid]
    }

q2_aYes_fuQuestion1Plain' :: Question
q2_aYes_fuQuestion1Plain' = OptionsQuestion' q2_aYes_fuQuestion1Plain

q2_aYes_fuQuestion1Plain :: OptionsQuestion
q2_aYes_fuQuestion1Plain =
  OptionsQuestion
    { _optionsQuestionUuid = q2_aYes_fuQuestion1 ^. uuid
    , _optionsQuestionTitle = "Fourth Question"
    , _optionsQuestionText = Just "Just follow"
    , _optionsQuestionRequiredPhaseUuid = Just $ PHS.phase2 ^. uuid
    , _optionsQuestionAnnotations = M.empty
    , _optionsQuestionTagUuids = []
    , _optionsQuestionReferenceUuids = []
    , _optionsQuestionExpertUuids = []
    , _optionsQuestionAnswerUuids = []
    }

-- -----------------------------------------------------------------------------
q2_aYes_fuq1_aYes_fuQuestion2' :: Question
q2_aYes_fuq1_aYes_fuQuestion2' = OptionsQuestion' q2_aYes_fuq1_aYes_fuQuestion2

q2_aYes_fuq1_aYes_fuQuestion2 :: OptionsQuestion
q2_aYes_fuq1_aYes_fuQuestion2 =
  OptionsQuestion
    { _optionsQuestionUuid = u' "393eb40a-27bd-4156-9b2d-c4e8c582cca8"
    , _optionsQuestionTitle = "Second Follow-Up Question"
    , _optionsQuestionText = Just "Again just follow"
    , _optionsQuestionRequiredPhaseUuid = Just $ PHS.phase2 ^. uuid
    , _optionsQuestionAnnotations = M.empty
    , _optionsQuestionTagUuids = []
    , _optionsQuestionReferenceUuids = []
    , _optionsQuestionExpertUuids = []
    , _optionsQuestionAnswerUuids = [q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid]
    }

q2_aYes_fuq1_aYes_fuQuestion2Edited' :: Question
q2_aYes_fuq1_aYes_fuQuestion2Edited' = OptionsQuestion' q2_aYes_fuq1_aYes_fuQuestion2Edited

q2_aYes_fuq1_aYes_fuQuestion2Edited :: OptionsQuestion
q2_aYes_fuq1_aYes_fuQuestion2Edited =
  OptionsQuestion
    { _optionsQuestionUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _optionsQuestionTitle = "EDITED: Second Follow-Up Question"
    , _optionsQuestionText = Just "EDITED: Again just follow"
    , _optionsQuestionRequiredPhaseUuid = Just $ PHS.phase1 ^. uuid
    , _optionsQuestionAnnotations = M.empty
    , _optionsQuestionTagUuids = []
    , _optionsQuestionReferenceUuids = []
    , _optionsQuestionExpertUuids = []
    , _optionsQuestionAnswerUuids = [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid]
    }

-- -----------------------------------------------------------------------------
q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3' :: Question
q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3' = OptionsQuestion' q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3

q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 :: OptionsQuestion
q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 =
  OptionsQuestion
    { _optionsQuestionUuid = u' "70b6a446-bd35-4d5e-8995-78a94a69da83"
    , _optionsQuestionTitle = "Third Follow-Up Question"
    , _optionsQuestionText = Just "Again and again just follow"
    , _optionsQuestionRequiredPhaseUuid = Just $ PHS.phase2 ^. uuid
    , _optionsQuestionAnnotations = M.empty
    , _optionsQuestionTagUuids = []
    , _optionsQuestionReferenceUuids = []
    , _optionsQuestionExpertUuids = []
    , _optionsQuestionAnswerUuids = []
    }

-- -----------------------------------------------------------------------------
q4_it1_q6_aYes_followUpQuestion4' :: Question
q4_it1_q6_aYes_followUpQuestion4' = ListQuestion' q4_it1_q6_aYes_followUpQuestion4

q4_it1_q6_aYes_followUpQuestion4 :: ListQuestion
q4_it1_q6_aYes_followUpQuestion4 =
  ListQuestion
    { _listQuestionUuid = u' "cd98f76a-a430-4bd6-ba63-eb4c3c5c8c7e"
    , _listQuestionTitle = "Fourth Follow-Up Question"
    , _listQuestionText = Just "Again and again just follow"
    , _listQuestionRequiredPhaseUuid = Just $ PHS.phase2 ^. uuid
    , _listQuestionAnnotations = M.empty
    , _listQuestionTagUuids = []
    , _listQuestionReferenceUuids = []
    , _listQuestionExpertUuids = []
    , _listQuestionItemTemplateQuestionUuids =
        [q4_it1_q6_aYes_fuq4_it_question1 ^. uuid, q4_it1_q6_aYes_fuq4_it_question2 ^. uuid]
    }

q4_it1_q6_aYes_followUpQuestion4Edited' :: Question
q4_it1_q6_aYes_followUpQuestion4Edited' = ListQuestion' q4_it1_q6_aYes_followUpQuestion4Edited

q4_it1_q6_aYes_followUpQuestion4Edited :: ListQuestion
q4_it1_q6_aYes_followUpQuestion4Edited =
  ListQuestion
    { _listQuestionUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
    , _listQuestionTitle = "EDITED: Third Follow-Up Question"
    , _listQuestionText = Just "EDITED: Again and again just follow"
    , _listQuestionRequiredPhaseUuid = Just $ PHS.phase1 ^. uuid
    , _listQuestionAnnotations = M.empty
    , _listQuestionTagUuids = []
    , _listQuestionReferenceUuids = []
    , _listQuestionExpertUuids = []
    , _listQuestionItemTemplateQuestionUuids =
        [q4_it1_q6_aYes_fuq4_it_question2 ^. uuid, q4_it1_q6_aYes_fuq4_it_question1 ^. uuid]
    }

q4_it1_q6_aYes_fuq4_it_question1' :: Question
q4_it1_q6_aYes_fuq4_it_question1' = OptionsQuestion' q4_it1_q6_aYes_fuq4_it_question1

q4_it1_q6_aYes_fuq4_it_question1 :: OptionsQuestion
q4_it1_q6_aYes_fuq4_it_question1 =
  OptionsQuestion
    { _optionsQuestionUuid = u' "e5a3e1b2-077a-405f-b35c-3bffded63140"
    , _optionsQuestionTitle = "Sub question 1 of Follow-Up Question 4"
    , _optionsQuestionText = Just "Again and again just follow"
    , _optionsQuestionRequiredPhaseUuid = Just $ PHS.phase2 ^. uuid
    , _optionsQuestionAnnotations = M.empty
    , _optionsQuestionTagUuids = []
    , _optionsQuestionReferenceUuids = []
    , _optionsQuestionExpertUuids = []
    , _optionsQuestionAnswerUuids = []
    }

q4_it1_q6_aYes_fuq4_it_question2' :: Question
q4_it1_q6_aYes_fuq4_it_question2' = OptionsQuestion' q4_it1_q6_aYes_fuq4_it_question2

q4_it1_q6_aYes_fuq4_it_question2 :: OptionsQuestion
q4_it1_q6_aYes_fuq4_it_question2 =
  OptionsQuestion
    { _optionsQuestionUuid = u' "7f2e3fe5-b8b6-4b5a-812d-c5c1c704b3d9"
    , _optionsQuestionTitle = "Sub question 2 of Follow-Up Question 4"
    , _optionsQuestionText = Just "Again and again just follow"
    , _optionsQuestionRequiredPhaseUuid = Just $ PHS.phase2 ^. uuid
    , _optionsQuestionAnnotations = M.empty
    , _optionsQuestionTagUuids = []
    , _optionsQuestionReferenceUuids = []
    , _optionsQuestionExpertUuids = []
    , _optionsQuestionAnswerUuids = []
    }

q4_it1_q6_aYes_followUpQuestion5' :: Question
q4_it1_q6_aYes_followUpQuestion5' = IntegrationQuestion' q4_it1_q6_aYes_followUpQuestion5

q4_it1_q6_aYes_followUpQuestion5 :: IntegrationQuestion
q4_it1_q6_aYes_followUpQuestion5 =
  IntegrationQuestion
    { _integrationQuestionUuid = u' "82f9a83a-88c8-439b-8cf8-8a028d5cce7d"
    , _integrationQuestionTitle = "Fifth Follow-Up Question"
    , _integrationQuestionText = Just "Some non-funny description"
    , _integrationQuestionRequiredPhaseUuid = Just $ PHS.phase2 ^. uuid
    , _integrationQuestionAnnotations = M.empty
    , _integrationQuestionTagUuids = []
    , _integrationQuestionReferenceUuids = []
    , _integrationQuestionExpertUuids = []
    , _integrationQuestionIntegrationUuid = FI.ontologyPortal ^. uuid
    , _integrationQuestionProps = Map.fromList [("domain", "biology"), ("country", "be")]
    }

q4_it1_q6_aYes_fuq5PropsEdited' :: Question
q4_it1_q6_aYes_fuq5PropsEdited' = IntegrationQuestion' q4_it1_q6_aYes_fuq5PropsEdited

q4_it1_q6_aYes_fuq5PropsEdited :: IntegrationQuestion
q4_it1_q6_aYes_fuq5PropsEdited =
  IntegrationQuestion
    { _integrationQuestionUuid = q4_it1_q6_aYes_followUpQuestion5 ^. uuid
    , _integrationQuestionTitle = q4_it1_q6_aYes_followUpQuestion5 ^. title
    , _integrationQuestionText = q4_it1_q6_aYes_followUpQuestion5 ^. text
    , _integrationQuestionRequiredPhaseUuid = q4_it1_q6_aYes_followUpQuestion5 ^. requiredPhaseUuid
    , _integrationQuestionAnnotations = M.empty
    , _integrationQuestionTagUuids = q4_it1_q6_aYes_followUpQuestion5 ^. tagUuids
    , _integrationQuestionReferenceUuids = q4_it1_q6_aYes_followUpQuestion5 ^. referenceUuids
    , _integrationQuestionExpertUuids = q4_it1_q6_aYes_followUpQuestion5 ^. expertUuids
    , _integrationQuestionIntegrationUuid = q4_it1_q6_aYes_followUpQuestion5 ^. integrationUuid
    , _integrationQuestionProps = Map.fromList [("domain", "biology"), ("language", "")]
    }

q4_it1_q6_aYes_fuq5ConvertedToValue' :: Question
q4_it1_q6_aYes_fuq5ConvertedToValue' = ValueQuestion' q4_it1_q6_aYes_fuq5ConvertedToValue

q4_it1_q6_aYes_fuq5ConvertedToValue :: ValueQuestion
q4_it1_q6_aYes_fuq5ConvertedToValue =
  ValueQuestion
    { _valueQuestionUuid = q4_it1_q6_aYes_followUpQuestion5 ^. uuid
    , _valueQuestionTitle = q4_it1_q6_aYes_followUpQuestion5 ^. title
    , _valueQuestionText = q4_it1_q6_aYes_followUpQuestion5 ^. text
    , _valueQuestionRequiredPhaseUuid = q4_it1_q6_aYes_followUpQuestion5 ^. requiredPhaseUuid
    , _valueQuestionAnnotations = M.empty
    , _valueQuestionTagUuids = q4_it1_q6_aYes_followUpQuestion5 ^. tagUuids
    , _valueQuestionReferenceUuids = q4_it1_q6_aYes_followUpQuestion5 ^. referenceUuids
    , _valueQuestionExpertUuids = q4_it1_q6_aYes_followUpQuestion5 ^. expertUuids
    , _valueQuestionValueType = StringQuestionValueType
    }
