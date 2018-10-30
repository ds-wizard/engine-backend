module Service.Report.ReportGenerator where

import Control.Lens ((^.))
import Data.Maybe (catMaybes, isJust)
import Data.Time

import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.FilledKnowledgeModel.FilledKnowledgeModelAccessors
import Model.KnowledgeModel.KnowledgeModel
import Model.Report.Report
import Util.Math
import Util.Uuid

isQuestionAnswered :: FilledQuestion -> Bool
isQuestionAnswered fq =
  case fq ^. qType of
    QuestionTypeOptions -> isJust $ fq ^. answerOption
    QuestionTypeList -> isJust $ fq ^. answerItems
    QuestionTypeString -> isJust $ fq ^. answerValue
    QuestionTypeNumber -> isJust $ fq ^. answerValue
    QuestionTypeDate -> isJust $ fq ^. answerValue
    QuestionTypeText -> isJust $ fq ^. answerValue

computeAnsweredIndication :: Int -> FilledChapter -> Indication
computeAnsweredIndication currentLevel fChapter =
  AnsweredIndication' $
  AnsweredIndication
  { _answeredIndicationAnsweredQuestions = sum $ getQuestionCount (isQuestionAnswered) <$> fChapter ^. questions
  , _answeredIndicationUnansweredQuestions = sum $ getQuestionCount (not . isQuestionAnswered) <$> fChapter ^. questions
  }
  where
    getQuestionCount :: (FilledQuestion -> Bool) -> FilledQuestion -> Int
    getQuestionCount condition fq = currentQuestion + childrens
      where
        currentQuestion =
          if condition fq && isRequiredNow
            then 1
            else 0
        isRequiredNow =
          case fq ^. requiredLevel of
            Just rl -> rl <= currentLevel
            Nothing -> True
        childrens = (walkOverAnswerOption $ fq ^. answerOption) + (walkOverAnswerItems $ fq ^. answerItems)
          where
            walkOverAnswerOption mAo = sum $ maybe [] (\ao -> (getQuestionCount condition) <$> ao ^. followUps) mAo
            walkOverAnswerItems mAis = sum $ maybe [] (\ais -> walkOverAnswerItem <$> ais) mAis
            walkOverAnswerItem ai = sum $ (getQuestionCount condition) <$> ai ^. questions

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
computeMetricSummary :: FilledChapter -> Metric -> MetricSummary
computeMetricSummary fChapter m =
  MetricSummary {_metricSummaryMetricUuid = m ^. uuid, _metricSummaryMeasure = msMeasure}
  where
    msMeasure :: Double
    msMeasure =
      weightAverage .
      mapToTouple .
      filterAccordingCurrentMetric . mapToMetricMeasures . filterOptionsType . getAllFilledQuestionsForChapter $
      fChapter
    filterOptionsType :: [FilledQuestion] -> [FilledQuestion]
    filterOptionsType = filter (\fq -> fq ^. qType == QuestionTypeOptions)
    mapToMetricMeasures :: [FilledQuestion] -> [MetricMeasure]
    mapToMetricMeasures = concat . (map _filledAnswerMetricMeasures) . catMaybes . (map _filledQuestionAnswerOption)
    filterAccordingCurrentMetric :: [MetricMeasure] -> [MetricMeasure]
    filterAccordingCurrentMetric = filter (\mm -> mm ^. metricUuid == m ^. uuid)
    mapToTouple :: [MetricMeasure] -> [(Double, Double)]
    mapToTouple = map (\mm -> (mm ^. measure, mm ^. weight))

computeMetrics :: [Metric] -> FilledChapter -> [MetricSummary]
computeMetrics metrics fChapter = (computeMetricSummary fChapter) <$> metrics

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
computeChapterReport :: Int -> [Metric] -> FilledChapter -> ChapterReport
computeChapterReport currentLevel metrics fChapter =
  ChapterReport
  { _chapterReportChapterUuid = fChapter ^. uuid
  , _chapterReportIndications = [computeAnsweredIndication currentLevel fChapter]
  , _chapterReportMetrics = computeMetrics metrics fChapter
  }

generateReport :: Int -> [Metric] -> FilledKnowledgeModel -> IO Report
generateReport currentLevel metrics filledKM = do
  rUuid <- generateUuid
  now <- getCurrentTime
  return
    Report
    { _reportUuid = rUuid
    , _reportChapterReports = (computeChapterReport currentLevel metrics) <$> (filledKM ^. chapters)
    , _reportCreatedAt = now
    , _reportUpdatedAt = now
    }
