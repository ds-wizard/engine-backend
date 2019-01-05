module Service.Report.ReportGenerator where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Maybe (catMaybes, isJust)
import Data.Time

import LensesConfig
import Model.Context.AppContext
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
    QuestionTypeList ->
      case fq ^. answerItems of
        Nothing -> False
        Just [] -> False
        Just (x:xs) -> True
    QuestionTypeString -> isJust $ fq ^. answerValue
    QuestionTypeNumber -> isJust $ fq ^. answerValue
    QuestionTypeDate -> isJust $ fq ^. answerValue
    QuestionTypeText -> isJust $ fq ^. answerValue

isAIAnswered :: FilledAnswerItem -> Bool
isAIAnswered fai = isJust $ fai ^. value

computeAnsweredIndication :: Int -> FilledChapter -> Indication
computeAnsweredIndication currentLevel fChapter =
  AnsweredIndication' $
  AnsweredIndication
  { _answeredIndicationAnsweredQuestions =
      sum $ getQuestionCount (isQuestionAnswered) (isAIAnswered) <$> fChapter ^. questions
  , _answeredIndicationUnansweredQuestions =
      sum $ getQuestionCount (not . isQuestionAnswered) (not . isAIAnswered) <$> fChapter ^. questions
  }
  where
    getQuestionCount :: (FilledQuestion -> Bool) -> (FilledAnswerItem -> Bool) -> FilledQuestion -> Int
    getQuestionCount conditionQ conditionAI fq = currentQuestion + childrens
      where
        currentQuestion =
          if conditionQ fq && isRequiredNow
            then 1
            else 0
        isRequiredNow =
          case fq ^. requiredLevel of
            Just rl -> rl <= currentLevel
            Nothing -> True
        childrens = (walkOverAnswerOption $ fq ^. answerOption) + (walkOverAnswerItems $ fq ^. answerItems)
          where
            walkOverAnswerOption mAo =
              sum $ maybe [] (\ao -> (getQuestionCount conditionQ conditionAI) <$> ao ^. followUps) mAo
            walkOverAnswerItems mAis = sum $ maybe [] (\ais -> walkOverAnswerItem <$> ais) mAis
            walkOverAnswerItem ai =
              let itemName =
                    if conditionAI ai && isRequiredNow
                      then 1
                      else 0
                  questionsCount = (sum $ (getQuestionCount conditionQ conditionAI) <$> ai ^. questions)
              in itemName + questionsCount

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

generateReport :: Int -> [Metric] -> FilledKnowledgeModel -> AppContextM Report
generateReport currentLevel metrics filledKM = do
  rUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  return
    Report
    { _reportUuid = rUuid
    , _reportChapterReports = (computeChapterReport currentLevel metrics) <$> (filledKM ^. chapters)
    , _reportCreatedAt = now
    , _reportUpdatedAt = now
    }
