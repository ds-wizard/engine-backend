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

computeAnsweredIndication :: FilledChapter -> Indication
computeAnsweredIndication fChapter =
  AnsweredIndication' $
  AnsweredIndication
  {_answeredIndicationAnsweredQuestions = answered, _answeredIndicationUnansweredQuestions = unanswered}
  where
    answered :: Int
    answered = foldl (+) 0 $ getAnsweredQuestion (isQuestionAnswered) <$> fChapter ^. questions
    unanswered = foldl (+) 0 $ getAnsweredQuestion (not . isQuestionAnswered) <$> fChapter ^. questions
    getAnsweredQuestion :: (FilledQuestion -> Bool) -> FilledQuestion -> Int
    getAnsweredQuestion condition fQuestion = current + childrens
      where
        current =
          if condition fQuestion
            then 1
            else 0
        childrens =
          (walkThroughAnswerOption $ fQuestion ^. answerOption) + (walkThroughAnswerItems $ fQuestion ^. answerItems)
        walkThroughAnswerOption mAo =
          foldl (+) 0 $
          case mAo of
            Just ao -> (getAnsweredQuestion condition) <$> ao ^. followUps
            Nothing -> []
        walkThroughAnswerItems mAis =
          foldl (+) 0 $
          case mAis of
            Just ais -> walkThoughAnswerItem <$> ais
            Nothing -> []
        walkThoughAnswerItem ai = foldl (+) 0 $ (getAnsweredQuestion condition) <$> ai ^. questions

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
computeMetricSummaryF :: FilledChapter -> Metric -> MetricSummary
computeMetricSummaryF fChapter m =
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
    -- iMeasure :: Double
    -- iMeasure = weightAverage ((\mm -> (mm ^. measure, mm ^. weight)) <$> measures)
    -- measures :: [MetricMeasure]
    -- measures = catMaybes . concat $ getQuestionMeasure <$> fChapter ^. questions
    -- getQuestionMeasure :: FilledQuestion -> [Maybe MetricMeasure]
    -- getQuestionMeasure fQuestion = [current] ++ childrens
    --   where
    --     current = case fQuestion ^. qType of
    --       QuestionTypeOptions ->  case fQuestion ^. answerOption of
    --         Just ao -> find (m==) (ao ^. metricMeasures)
    --         Nothing -> Nothing
    --       _ -> Nothing
    --     childrens =
    --       (walkThroughAnswerOption $ fQuestion ^. answerOption) ++ (walkThroughAnswerItems $ fQuestion ^. answerItems)
    --     walkThroughAnswerOption mAo =
    --       concat $
    --       case mAo of
    --         Just ao -> getQuestionMeasure <$> ao ^. followUps
    --         Nothing -> []
    --     walkThroughAnswerItems mAis =
    --       concat $
    --       case mAis of
    --         Just ais -> walkThoughAnswerItem <$> ais
    --         Nothing -> []
    --     walkThoughAnswerItem ai = concat $ getQuestionMeasure <$> ai ^. questions

computeMetrics :: [Metric] -> FilledChapter -> [MetricSummary]
computeMetrics metrics fChapter =
  let indicationF = computeMetricSummaryF fChapter (metrics !! 0)
  in [indicationF]

computeChapterReport :: [Metric] -> FilledChapter -> ChapterReport
computeChapterReport metrics fChapter =
  ChapterReport
  { _chapterReportChapterUuid = fChapter ^. uuid
  , _chapterReportIndications = [computeAnsweredIndication fChapter]
  , _chapterReportMetrics = computeMetrics metrics fChapter
  }

generateReport :: [Metric] -> FilledKnowledgeModel -> IO Report
generateReport metrics filledKM = do
  rUuid <- generateUuid
  now <- getCurrentTime
  return
    Report
    { _reportUuid = rUuid
    , _reportChapterReports = (computeChapterReport metrics) <$> (filledKM ^. chapters)
    , _reportCreatedAt = now
    , _reportUpdatedAt = now
    }
