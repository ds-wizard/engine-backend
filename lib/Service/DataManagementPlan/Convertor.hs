module Service.DataManagementPlan.Convertor where

import Control.Lens ((^.))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Text.Numeral.Roman (toRoman)
import Util.Alphabet (convertToLetter)

import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.KnowledgeModel.KnowledgeModel

toFilledKM :: KnowledgeModel -> FilledKnowledgeModel
toFilledKM km =
  FilledKnowledgeModel
  { _filledKnowledgeModelUuid = km ^. uuid
  , _filledKnowledgeModelName = km ^. name
  , _filledKnowledgeModelChapters = toFilledChapter <$> (zip humanIdentifiers (km ^. chapters))
  , _filledKnowledgeModelTags = km ^. tags
  , _filledKnowledgeModelIntegrations = km ^. integrations
  }
  where
    humanIdentifiers :: [String]
    humanIdentifiers = generateRomanHumanIdentifiers . length $ km ^. chapters

toFilledChapter :: (String, Chapter) -> FilledChapter
toFilledChapter (humanIdentifier, ch) =
  FilledChapter
  { _filledChapterUuid = ch ^. uuid
  , _filledChapterHumanIdentifier = humanIdentifier
  , _filledChapterTitle = ch ^. title
  , _filledChapterText = ch ^. text
  , _filledChapterQuestions = toFilledQuestion' <$> (zip humanIdentifiers (ch ^. questions))
  }
  where
    humanIdentifiers :: [String]
    humanIdentifiers = generateNumberedHumanIdentifiers "" . length $ ch ^. questions

toFilledQuestion' :: (String, Question) -> FilledQuestion
toFilledQuestion' (humanIdentifier, question) = toFilledQuestion humanIdentifier question

toFilledQuestion :: String -> Question -> FilledQuestion
toFilledQuestion humanIdentifier (OptionsQuestion' q) =
  FilledOptionsQuestion' $
  FilledOptionsQuestion
  { _filledOptionsQuestionUuid = q ^. uuid
  , _filledOptionsQuestionHumanIdentifier = humanIdentifier
  , _filledOptionsQuestionTitle = q ^. title
  , _filledOptionsQuestionText = q ^. text
  , _filledOptionsQuestionRequiredLevel = q ^. requiredLevel
  , _filledOptionsQuestionTagUuids = q ^. tagUuids
  , _filledOptionsQuestionExperts = q ^. experts
  , _filledOptionsQuestionReferences = q ^. references
  , _filledOptionsQuestionAnswers = q ^. answers
  , _filledOptionsQuestionAnswerOption = Nothing
  }
toFilledQuestion humanIdentifier (ListQuestion' q) =
  FilledListQuestion' $
  FilledListQuestion
  { _filledListQuestionUuid = q ^. uuid
  , _filledListQuestionHumanIdentifier = humanIdentifier
  , _filledListQuestionTitle = q ^. title
  , _filledListQuestionText = q ^. text
  , _filledListQuestionRequiredLevel = q ^. requiredLevel
  , _filledListQuestionTagUuids = q ^. tagUuids
  , _filledListQuestionExperts = q ^. experts
  , _filledListQuestionReferences = q ^. references
  , _filledListQuestionItemTemplateTitle = q ^. itemTemplateTitle
  , _filledListQuestionItemTemplateQuestions = q ^. itemTemplateQuestions
  , _filledListQuestionItems = Nothing
  }
toFilledQuestion humanIdentifier (ValueQuestion' q) =
  FilledValueQuestion' $
  FilledValueQuestion
  { _filledValueQuestionUuid = q ^. uuid
  , _filledValueQuestionHumanIdentifier = humanIdentifier
  , _filledValueQuestionTitle = q ^. title
  , _filledValueQuestionText = q ^. text
  , _filledValueQuestionRequiredLevel = q ^. requiredLevel
  , _filledValueQuestionTagUuids = q ^. tagUuids
  , _filledValueQuestionExperts = q ^. experts
  , _filledValueQuestionReferences = q ^. references
  , _filledValueQuestionValueType = q ^. valueType
  , _filledValueQuestionAnswerValue = Nothing
  }
toFilledQuestion humanIdentifier (IntegrationQuestion' q) =
  FilledIntegrationQuestion' $
  FilledIntegrationQuestion
  { _filledIntegrationQuestionUuid = q ^. uuid
  , _filledIntegrationQuestionHumanIdentifier = humanIdentifier
  , _filledIntegrationQuestionTitle = q ^. title
  , _filledIntegrationQuestionText = q ^. text
  , _filledIntegrationQuestionRequiredLevel = q ^. requiredLevel
  , _filledIntegrationQuestionTagUuids = q ^. tagUuids
  , _filledIntegrationQuestionExperts = q ^. experts
  , _filledIntegrationQuestionReferences = q ^. references
  , _filledIntegrationQuestionIntegrationUuid = q ^. integrationUuid
  , _filledIntegrationQuestionProps = q ^. props
  , _filledIntegrationQuestionAnswerIntId = Nothing
  , _filledIntegrationQuestionAnswerValue = Nothing
  }

toFilledAnswer :: FilledOptionsQuestion -> Answer -> FilledAnswer
toFilledAnswer fQ ans =
  FilledAnswer
  { _filledAnswerUuid = ans ^. uuid
  , _filledAnswerHumanIdentifier = ansHumanIdentifier
  , _filledAnswerLabel = ans ^. label
  , _filledAnswerAdvice = ans ^. advice
  , _filledAnswerFollowUps = toFilledQuestion' <$> (zip humanIdentifiers (ans ^. followUps))
  , _filledAnswerMetricMeasures = ans ^. metricMeasures
  }
  where
    ansHumanIdentifier :: String
    ansHumanIdentifier = [convertToLetter $ (fromMaybe 0 (elemIndex ans (fQ ^. answers))) + 1]
    prefixHumanIdentifier :: String
    prefixHumanIdentifier = (fQ ^. humanIdentifier) ++ "." ++ ansHumanIdentifier
    humanIdentifiers :: [String]
    humanIdentifiers = generateNumberedHumanIdentifiers prefixHumanIdentifier . length $ ans ^. followUps

toFilledAnswerItem :: Int -> FilledListQuestion -> FilledAnswerItem
toFilledAnswerItem number fQ =
  FilledAnswerItem
  { _filledAnswerItemHumanIdentifier = aiHumanIdentifier
  , _filledAnswerItemTitle = fQ ^. itemTemplateTitle
  , _filledAnswerItemValue = Nothing
  , _filledAnswerItemQuestions = toFilledQuestion' <$> (zip humanIdentifiers (fQ ^. itemTemplateQuestions))
  }
  where
    aiHumanIdentifier :: String
    aiHumanIdentifier = (fQ ^. humanIdentifier) ++ "." ++ [convertToLetter (number + 1)]
    humanIdentifiers :: [String]
    humanIdentifiers = generateNumberedHumanIdentifiers aiHumanIdentifier . length $ fQ ^. itemTemplateQuestions

-- --------------------------------
-- PRIVATE
-- --------------------------------
generateRomanHumanIdentifiers :: Int -> [String]
generateRomanHumanIdentifiers size = (\number -> toRoman number) <$> [1 .. size]

generateNumberedHumanIdentifiers :: String -> Int -> [String]
generateNumberedHumanIdentifiers "" size = (\number -> show number) <$> [1 .. size]
generateNumberedHumanIdentifiers prefix size = (\number -> prefix ++ "." ++ show number) <$> [1 .. size]
