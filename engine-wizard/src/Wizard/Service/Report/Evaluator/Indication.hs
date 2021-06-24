module Wizard.Service.Report.Evaluator.Indication
  ( computeIndications
  ) where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelAccessors
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Service.Report.Evaluator.Common

computeIndications :: Bool -> U.UUID -> KnowledgeModel -> [ReplyTuple] -> Chapter -> [Indication]
computeIndications levelsEnabled qtnPhase km replies ch =
  if levelsEnabled
    then [computeLevelsAnsweredIndication qtnPhase km replies ch, computeAnsweredIndication U.nil km replies ch]
    else [computeAnsweredIndication U.nil km replies ch]

-- --------------------------------
-- PRIVATE
-- --------------------------------
computeAnsweredIndication :: U.UUID -> KnowledgeModel -> [ReplyTuple] -> Chapter -> Indication
computeAnsweredIndication qtnPhase km replies ch =
  AnsweredIndication' $
  AnsweredIndication
    { _answeredIndicationAnsweredQuestions = evaluateChapter 1 0 qtnPhase km replies ch
    , _answeredIndicationUnansweredQuestions = evaluateChapter 0 1 qtnPhase km replies ch
    }

computeLevelsAnsweredIndication :: U.UUID -> KnowledgeModel -> [ReplyTuple] -> Chapter -> Indication
computeLevelsAnsweredIndication qtnPhase km replies ch =
  LevelsAnsweredIndication' $
  LevelsAnsweredIndication
    { _levelsAnsweredIndicationAnsweredQuestions = evaluateChapter 1 0 qtnPhase km replies ch
    , _levelsAnsweredIndicationUnansweredQuestions = evaluateChapter 0 1 qtnPhase km replies ch
    }

evaluateChapter :: Int -> Int -> U.UUID -> KnowledgeModel -> [ReplyTuple] -> Chapter -> Int
evaluateChapter found notFound qtnPhase km replies ch =
  let currentPath = U.toString $ ch ^. uuid
      qs = getQuestionsForChapterUuid km (ch ^. uuid)
   in sum . fmap (evaluateQuestion found notFound qtnPhase km replies currentPath) $ qs

evaluateQuestion :: Int -> Int -> U.UUID -> KnowledgeModel -> [ReplyTuple] -> String -> Question -> Int
evaluateQuestion found notFound qtnPhase km replies path q' =
  let currentPath = composePathUuid path $ q' ^. uuid'
   in case getReply replies currentPath of
        Just reply -> children currentPath
        Nothing -> isRequiredNow (km ^. phaseUuids) (q' ^. requiredPhaseUuid') qtnPhase notFound
  where
    children currentPath =
      case q' of
        MultiChoiceQuestion' q -> evaluateMultiChoiceQuestion q found notFound qtnPhase km replies currentPath
        ValueQuestion' q -> rFound
        IntegrationQuestion' q -> rFound
        OptionsQuestion' q -> rFound + evaluateOptionsQuestion q found notFound qtnPhase km replies currentPath
        ListQuestion' q -> evaluateListQuestion found notFound qtnPhase km replies currentPath q
      where
        rFound = isRequiredNow (km ^. phaseUuids) (q' ^. requiredPhaseUuid') qtnPhase found

evaluateOptionsQuestion :: OptionsQuestion -> Int -> Int -> U.UUID -> KnowledgeModel -> [ReplyTuple] -> String -> Int
evaluateOptionsQuestion q found notFound qtnPhase km replies path =
  case getReply replies path of
    Just (_, Reply {_replyValue = AnswerReply {..}}) ->
      let currentPath = composePathUuid path _answerReplyValue
          qs = getQuestionsForAnswerUuid km _answerReplyValue
       in sum . fmap (evaluateQuestion found notFound qtnPhase km replies currentPath) $ qs
    Nothing -> isRequiredNow (km ^. phaseUuids) (q ^. requiredPhaseUuid) qtnPhase notFound

evaluateListQuestion :: Int -> Int -> U.UUID -> KnowledgeModel -> [ReplyTuple] -> String -> ListQuestion -> Int
evaluateListQuestion found notFound qtnPhase km replies currentPath q =
  let itemQs = getItemTemplateQuestionsForQuestionUuid km $ q ^. uuid
      items =
        case getReply replies currentPath of
          Just (_, Reply {_replyValue = ItemListReply {..}}) -> _itemListReplyValue
          _ -> []
      evaluateQuestion' item =
        fmap (evaluateQuestion found notFound qtnPhase km replies (composePath currentPath $ U.toString item)) itemQs
      current =
        if not (null items)
          then isRequiredNow (km ^. phaseUuids) (q ^. requiredPhaseUuid) qtnPhase found
          else isRequiredNow (km ^. phaseUuids) (q ^. requiredPhaseUuid) qtnPhase notFound
      childrens = sum . concatMap evaluateQuestion' $ items
   in current + childrens

evaluateMultiChoiceQuestion ::
     MultiChoiceQuestion -> Int -> Int -> U.UUID -> KnowledgeModel -> [ReplyTuple] -> String -> Int
evaluateMultiChoiceQuestion q found notFound qtnPhase km replies path =
  case getReply replies path of
    Just (_, Reply {_replyValue = MultiChoiceReply {..}}) ->
      if not (null _multiChoiceReplyValue)
        then isRequiredNow (km ^. phaseUuids) (q ^. requiredPhaseUuid) qtnPhase found
        else isRequiredNow (km ^. phaseUuids) (q ^. requiredPhaseUuid) qtnPhase notFound
    Nothing -> isRequiredNow (km ^. phaseUuids) (q ^. requiredPhaseUuid) qtnPhase notFound
