module Wizard.Service.Report.Evaluator.Metric where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelAccessors
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Util.List (generateList)
import Shared.Util.Math
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Service.Report.Evaluator.Common

computeMetrics :: [Metric] -> KnowledgeModel -> [Reply] -> Maybe Chapter -> [MetricSummary]
computeMetrics metrics km replies mCh = fmap (computeMetric km replies mCh) metrics

computeMetric :: KnowledgeModel -> [Reply] -> Maybe Chapter -> Metric -> MetricSummary
computeMetric km replies mCh m =
  MetricSummary
    {_metricSummaryMetricUuid = m ^. uuid, _metricSummaryMeasure = weightAverage' . mapMetric . filterMetric $ measures}
  where
    weightAverage' :: [(Double, Double)] -> Maybe Double
    weightAverage' [] = Nothing
    weightAverage' xs = Just . weightAverage $ xs
    mapMetric :: [MetricMeasure] -> [(Double, Double)]
    mapMetric = fmap (\mm -> (mm ^. measure, mm ^. weight))
    filterMetric :: [MetricMeasure] -> [MetricMeasure]
    filterMetric = filter (\mm -> mm ^. metricUuid == m ^. uuid)
    measures :: [MetricMeasure]
    measures =
      case mCh of
        Nothing -> concatMap (evaluateChapter km replies) (getChaptersForKmUuid km)
        Just ch -> evaluateChapter km replies ch

-- --------------------------------
-- PRIVATE
-- --------------------------------
evaluateChapter :: KnowledgeModel -> [Reply] -> Chapter -> [MetricMeasure]
evaluateChapter km replies ch =
  let currentPath = U.toString $ ch ^. uuid
      qs = getQuestionsForChapterUuid km (ch ^. uuid)
   in concatMap (evaluateQuestion km replies currentPath) qs

evaluateQuestion :: KnowledgeModel -> [Reply] -> String -> Question -> [MetricMeasure]
evaluateQuestion km replies path q' =
  let currentPath = composePathUuid path $ q' ^. uuid'
   in case getReply replies currentPath of
        Just reply -> children currentPath
        Nothing -> []
  where
    children currentPath =
      case q' of
        ValueQuestion' q -> []
        IntegrationQuestion' q -> []
        OptionsQuestion' q -> evaluateOptionsQuestion q km replies currentPath
        ListQuestion' q -> evaluateListQuestion km replies currentPath q

evaluateOptionsQuestion :: OptionsQuestion -> KnowledgeModel -> [Reply] -> String -> [MetricMeasure]
evaluateOptionsQuestion q km replies path =
  case getReply replies path of
    Just Reply {_replyValue = AnswerReply {..}} ->
      case M.lookup _answerReplyValue (km ^. answersM) of
        Just answer ->
          let currentMeasures = answer ^. metricMeasures
              currentPath = composePathUuid path _answerReplyValue
              qs = getQuestionsForAnswerUuid km _answerReplyValue
           in currentMeasures ++ concatMap (evaluateQuestion km replies currentPath) qs
        Nothing -> []
    _ -> []

evaluateListQuestion :: KnowledgeModel -> [Reply] -> String -> ListQuestion -> [MetricMeasure]
evaluateListQuestion km replies currentPath q =
  let itemQs = getItemTemplateQuestionsForQuestionUuid km $ q ^. uuid
      itemCount =
        case getReply replies currentPath of
          Just Reply {_replyValue = ItemListReply {..}} -> _itemListReplyValue
          _ -> 0
      indexes = generateList itemCount
      evaluateQuestion' index = concatMap (evaluateQuestion km replies (composePath currentPath $ show index)) itemQs
   in concatMap evaluateQuestion' indexes
