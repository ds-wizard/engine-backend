module Wizard.Service.Report.Evaluator.Metric where

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.UUID as U

import Shared.Common.Util.Math
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Service.Report.Evaluator.Common
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessors
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

computeMetrics :: KnowledgeModel -> M.Map String Reply -> Maybe Chapter -> [MetricSummary]
computeMetrics km replies mCh =
  mapMaybe (filterEmptyMetricSummary . computeMetric km replies mCh) . getMetricsForKmUuid $ km

computeMetric :: KnowledgeModel -> M.Map String Reply -> Maybe Chapter -> Metric -> MetricSummary
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

evaluateChapter :: KnowledgeModel -> M.Map String Reply -> Chapter -> [MetricMeasure]
evaluateChapter km replies ch =
  let currentPath = U.toString $ ch.uuid
      qs = getQuestionsForChapterUuid km ch.uuid
   in concatMap (evaluateQuestion km replies currentPath) qs

evaluateQuestion :: KnowledgeModel -> M.Map String Reply -> String -> Question -> [MetricMeasure]
evaluateQuestion km replies path q' =
  let currentPath = composePathUuid path $ getUuid q'
   in case M.lookup currentPath replies of
        Just reply -> children currentPath
        Nothing -> []
  where
    children currentPath =
      case q' of
        MultiChoiceQuestion' q -> []
        ValueQuestion' q -> []
        IntegrationQuestion' q -> []
        ItemSelectQuestion' q -> []
        OptionsQuestion' q -> evaluateOptionsQuestion q km replies currentPath
        ListQuestion' q -> evaluateListQuestion km replies currentPath q
        FileQuestion' q -> []

evaluateOptionsQuestion :: OptionsQuestion -> KnowledgeModel -> M.Map String Reply -> String -> [MetricMeasure]
evaluateOptionsQuestion q km replies path =
  case M.lookup path replies of
    Just (Reply {value = AnswerReply {..}}) ->
      case M.lookup aValue (getAnswersM km) of
        Just answer ->
          let currentMeasures = answer.metricMeasures
              currentPath = composePathUuid path aValue
              qs = getQuestionsForAnswerUuid km aValue
           in currentMeasures ++ concatMap (evaluateQuestion km replies currentPath) qs
        Nothing -> []
    _ -> []

evaluateListQuestion :: KnowledgeModel -> M.Map String Reply -> String -> ListQuestion -> [MetricMeasure]
evaluateListQuestion km replies currentPath q =
  let itemQs = getItemTemplateQuestionsForQuestionUuid km $ q.uuid
      items =
        case M.lookup currentPath replies of
          Just (Reply {value = ItemListReply {..}}) -> ilValue
          _ -> []
      evaluateQuestion' item =
        concatMap (evaluateQuestion km replies (composePath currentPath $ U.toString item)) itemQs
   in concatMap evaluateQuestion' items
