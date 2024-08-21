module Wizard.Service.Report.Evaluator.Indication (
  computeIndications,
) where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Service.Report.Evaluator.Common
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessors
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

computeIndications :: Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> Chapter -> [Indication]
computeIndications mQtnPhase km replies ch =
  [computePhasesAnsweredIndication mQtnPhase km replies ch, computeAnsweredIndication Nothing km replies ch]

-- --------------------------------
-- PRIVATE
-- --------------------------------
computeAnsweredIndication :: Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> Chapter -> Indication
computeAnsweredIndication mQtnPhase km replies ch =
  AnsweredIndication' $
    AnsweredIndication
      { answeredQuestions = evaluateChapter 1 0 mQtnPhase km replies ch
      , unansweredQuestions = evaluateChapter 0 1 mQtnPhase km replies ch
      }

computePhasesAnsweredIndication :: Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> Chapter -> Indication
computePhasesAnsweredIndication mQtnPhase km replies ch =
  PhasesAnsweredIndication' $
    PhasesAnsweredIndication
      { answeredQuestions = evaluateChapter 1 0 mQtnPhase km replies ch
      , unansweredQuestions = evaluateChapter 0 1 mQtnPhase km replies ch
      }

evaluateChapter :: Int -> Int -> Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> Chapter -> Int
evaluateChapter found notFound mQtnPhase km replies ch =
  let currentPath = U.toString $ ch.uuid
      qs = getQuestionsForChapterUuid km ch.uuid
   in sum . fmap (evaluateQuestion found notFound mQtnPhase km replies currentPath) $ qs

evaluateQuestion :: Int -> Int -> Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> String -> Question -> Int
evaluateQuestion found notFound mQtnPhase km replies path q' =
  let currentPath = composePathUuid path $ getUuid q'
   in case M.lookup currentPath replies of
        Just reply -> children currentPath
        Nothing -> isRequiredNow km.phaseUuids (getRequiredPhaseUuid q') mQtnPhase notFound
  where
    children currentPath =
      case q' of
        MultiChoiceQuestion' q -> evaluateMultiChoiceQuestion q found notFound mQtnPhase km replies currentPath
        ValueQuestion' q -> rFound
        IntegrationQuestion' q -> rFound
        ItemSelectQuestion' q -> rFound
        OptionsQuestion' q -> rFound + evaluateOptionsQuestion q found notFound mQtnPhase km replies currentPath
        ListQuestion' q -> evaluateListQuestion found notFound mQtnPhase km replies currentPath q
      where
        rFound = isRequiredNow km.phaseUuids (getRequiredPhaseUuid q') mQtnPhase found

evaluateOptionsQuestion
  :: OptionsQuestion -> Int -> Int -> Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> String -> Int
evaluateOptionsQuestion q found notFound mQtnPhase km replies path =
  case M.lookup path replies of
    Just (Reply {value = AnswerReply {..}}) ->
      let currentPath = composePathUuid path aValue
          qs = getQuestionsForAnswerUuid km aValue
       in sum . fmap (evaluateQuestion found notFound mQtnPhase km replies currentPath) $ qs
    _ -> isRequiredNow km.phaseUuids q.requiredPhaseUuid mQtnPhase notFound

evaluateListQuestion :: Int -> Int -> Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> String -> ListQuestion -> Int
evaluateListQuestion found notFound mQtnPhase km replies currentPath q =
  let itemQs = getItemTemplateQuestionsForQuestionUuid km $ q.uuid
      items =
        case M.lookup currentPath replies of
          Just (Reply {value = ItemListReply {..}}) -> ilValue
          _ -> []
      evaluateQuestion' item =
        fmap (evaluateQuestion found notFound mQtnPhase km replies (composePath currentPath $ U.toString item)) itemQs
      current =
        if not (null items)
          then isRequiredNow km.phaseUuids q.requiredPhaseUuid mQtnPhase found
          else isRequiredNow km.phaseUuids q.requiredPhaseUuid mQtnPhase notFound
      childrens = sum . concatMap evaluateQuestion' $ items
   in current + childrens

evaluateMultiChoiceQuestion
  :: MultiChoiceQuestion -> Int -> Int -> Maybe U.UUID -> KnowledgeModel -> M.Map String Reply -> String -> Int
evaluateMultiChoiceQuestion q found notFound mQtnPhase km replies path =
  case M.lookup path replies of
    Just (Reply {value = MultiChoiceReply {..}}) ->
      if not (null mcValue)
        then isRequiredNow km.phaseUuids q.requiredPhaseUuid mQtnPhase found
        else isRequiredNow km.phaseUuids q.requiredPhaseUuid mQtnPhase notFound
    _ -> isRequiredNow km.phaseUuids q.requiredPhaseUuid mQtnPhase notFound
