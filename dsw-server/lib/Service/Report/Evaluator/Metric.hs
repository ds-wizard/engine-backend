module Service.Report.Evaluator.Metric where

import Control.Lens ((^.))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Model.Questionnaire.QuestionnaireReply
import Model.Report.Report
import Util.Math
import Util.String

computeMetrics :: [Metric] -> KnowledgeModel -> [Reply] -> Chapter -> [MetricSummary]
computeMetrics metrics km replies ch = fmap (computeMetricSummary km replies ch) metrics

computeMetricSummary :: KnowledgeModel -> [Reply] -> Chapter -> Metric -> MetricSummary
computeMetricSummary km replies ch m =
  MetricSummary {_metricSummaryMetricUuid = m ^. uuid, _metricSummaryMeasure = measure}
  where
    measure = weightAverage' . catMaybes . fmap (evaluateAnswer km ch m) $ replies
    weightAverage' [] = Nothing
    weightAverage' xs = Just . weightAverage $ xs

evaluateAnswer :: KnowledgeModel -> Chapter -> Metric -> Reply -> Maybe (Double, Double)
evaluateAnswer km ch m Reply {_replyPath = path, _replyValue = AnswerReply {..}} =
  if isFromChapter ch path
    then case M.lookup _answerReplyValue (km ^. answersM) of
           Just ans ->
             case L.find (\mm -> mm ^. metricUuid == m ^. uuid) (ans ^. metricMeasures) of
               Just mm -> Just (mm ^. measure, mm ^. weight)
               Nothing -> Nothing
    else Nothing
evaluateAnswer _ _ _ _ = Nothing

isFromChapter :: Chapter -> String -> Bool
isFromChapter ch path =
  case splitOn "." path of
    (chUuid:_) -> (U.toString $ ch ^. uuid) == chUuid
    _ -> False
