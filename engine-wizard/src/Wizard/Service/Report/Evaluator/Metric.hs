module Wizard.Service.Report.Evaluator.Metric where

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.UUID as U

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelAccessors
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Util.Math
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Service.Report.Evaluator.Common

computeMetrics :: KnowledgeModel -> [ReplyTuple] -> Maybe Chapter -> [MetricSummary]
computeMetrics km replies mCh =
  mapMaybe (filterEmptyMetricSummary . computeMetric km replies mCh) . getMetricsForKmUuid $ km

computeMetric :: KnowledgeModel -> [ReplyTuple] -> Maybe Chapter -> Metric -> MetricSummary
computeMetric km replies mCh m =
  MetricSummary
    { metricUuid = m.uuid
    , measure = weightAverage' . mapMetric . filterMetric $ measures
    }
  where
    weightAverage' :: [(Double, Double)] -> Maybe Double
    weightAverage' [] = Nothing
    weightAverage' xs = Just . weightAverage $ xs
    mapMetric :: [MetricMeasure] -> [(Double, Double)]
    mapMetric = fmap (\mm -> (mm.measure, mm.weight))
    filterMetric :: [MetricMeasure] -> [MetricMeasure]
    filterMetric = filter (\mm -> mm.metricUuid == m.uuid)
    measures :: [MetricMeasure]
    measures =
      case mCh of
        Nothing -> concatMap (evaluateChapter km replies) (getChaptersForKmUuid km)
        Just ch -> evaluateChapter km replies ch

-- --------------------------------
-- PRIVATE
-- --------------------------------
filterEmptyMetricSummary :: MetricSummary -> Maybe MetricSummary
filterEmptyMetricSummary ms =
  case ms.measure of
    Just msMeasure -> Just ms
    Nothing -> Nothing

evaluateChapter :: KnowledgeModel -> [ReplyTuple] -> Chapter -> [MetricMeasure]
evaluateChapter km replies ch =
  let currentPath = U.toString $ ch.uuid
      qs = getQuestionsForChapterUuid km ch.uuid
   in concatMap (evaluateQuestion km replies currentPath) qs

evaluateQuestion :: KnowledgeModel -> [ReplyTuple] -> String -> Question -> [MetricMeasure]
evaluateQuestion km replies path q' =
  let currentPath = composePathUuid path $ getUuid q'
   in case getReply replies currentPath of
        Just reply -> children currentPath
        Nothing -> []
  where
    children currentPath =
      case q' of
        MultiChoiceQuestion' q -> []
        ValueQuestion' q -> []
        IntegrationQuestion' q -> []
        OptionsQuestion' q -> evaluateOptionsQuestion q km replies currentPath
        ListQuestion' q -> evaluateListQuestion km replies currentPath q

evaluateOptionsQuestion :: OptionsQuestion -> KnowledgeModel -> [ReplyTuple] -> String -> [MetricMeasure]
evaluateOptionsQuestion q km replies path =
  case getReply replies path of
    Just (_, Reply {value = AnswerReply {..}}) ->
      case M.lookup aValue (getAnswersM km) of
        Just answer ->
          let currentMeasures = answer.metricMeasures
              currentPath = composePathUuid path aValue
              qs = getQuestionsForAnswerUuid km aValue
           in currentMeasures ++ concatMap (evaluateQuestion km replies currentPath) qs
        Nothing -> []
    _ -> []

evaluateListQuestion :: KnowledgeModel -> [ReplyTuple] -> String -> ListQuestion -> [MetricMeasure]
evaluateListQuestion km replies currentPath q =
  let itemQs = getItemTemplateQuestionsForQuestionUuid km $ q.uuid
      items =
        case getReply replies currentPath of
          Just (_, Reply {value = ItemListReply {..}}) -> ilValue
          _ -> []
      evaluateQuestion' item =
        concatMap (evaluateQuestion km replies (composePath currentPath $ U.toString item)) itemQs
   in concatMap evaluateQuestion' items
