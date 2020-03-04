module Wizard.Service.Report.Evaluator.Metric where

import Control.Lens ((^.))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Util.Math
import Shared.Util.String
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report

computeMetrics :: [Metric] -> KnowledgeModel -> [Reply] -> Maybe Chapter -> [MetricSummary]
computeMetrics metrics km replies mCh = fmap (computeMetricSummary km replies mCh) metrics

computeMetricSummary :: KnowledgeModel -> [Reply] -> Maybe Chapter -> Metric -> MetricSummary
computeMetricSummary km replies mCh m =
  MetricSummary {_metricSummaryMetricUuid = m ^. uuid, _metricSummaryMeasure = measure}
  where
    measure = weightAverage' . mapMaybe (evaluateAnswer km mCh m) $ replies
    weightAverage' [] = Nothing
    weightAverage' xs = Just . weightAverage $ xs

evaluateAnswer :: KnowledgeModel -> Maybe Chapter -> Metric -> Reply -> Maybe (Double, Double)
evaluateAnswer km mCh m Reply {_replyPath = path, _replyValue = AnswerReply {..}} =
  if isFromChapter mCh path
    then case M.lookup _answerReplyValue (km ^. answersM) of
           Just ans ->
             case L.find (\mm -> mm ^. metricUuid == m ^. uuid) (ans ^. metricMeasures) of
               Just mm -> Just (mm ^. measure, mm ^. weight)
               Nothing -> Nothing
    else Nothing
evaluateAnswer _ _ _ _ = Nothing

isFromChapter :: Maybe Chapter -> String -> Bool
isFromChapter mCh path =
  case mCh of
    Just ch ->
      case splitOn "." path of
        (chUuid:_) -> U.toString (ch ^. uuid) == chUuid
        _ -> False
    Nothing -> True
