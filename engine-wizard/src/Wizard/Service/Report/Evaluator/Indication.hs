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

computeIndications :: Bool -> Int -> KnowledgeModel -> [ReplyTuple] -> Chapter -> [Indication]
computeIndications levelsEnabled qtnLevel km replies ch =
  if levelsEnabled
    then [computeLevelsAnsweredIndication qtnLevel km replies ch, computeAnsweredIndication 9999 km replies ch]
    else [computeAnsweredIndication 9999 km replies ch]

-- --------------------------------
-- PRIVATE
-- --------------------------------
computeAnsweredIndication :: Int -> KnowledgeModel -> [ReplyTuple] -> Chapter -> Indication
computeAnsweredIndication qtnLevel km replies ch =
  AnsweredIndication' $
  AnsweredIndication
    { _answeredIndicationAnsweredQuestions = evaluateChapter 1 0 qtnLevel km replies ch
    , _answeredIndicationUnansweredQuestions = evaluateChapter 0 1 qtnLevel km replies ch
    }

computeLevelsAnsweredIndication :: Int -> KnowledgeModel -> [ReplyTuple] -> Chapter -> Indication
computeLevelsAnsweredIndication qtnLevel km replies ch =
  LevelsAnsweredIndication' $
  LevelsAnsweredIndication
    { _levelsAnsweredIndicationAnsweredQuestions = evaluateChapter 1 0 qtnLevel km replies ch
    , _levelsAnsweredIndicationUnansweredQuestions = evaluateChapter 0 1 qtnLevel km replies ch
    }

evaluateChapter :: Int -> Int -> Int -> KnowledgeModel -> [ReplyTuple] -> Chapter -> Int
evaluateChapter found notFound qtnLevel km replies ch =
  let currentPath = U.toString $ ch ^. uuid
      qs = getQuestionsForChapterUuid km (ch ^. uuid)
   in sum . fmap (evaluateQuestion found notFound qtnLevel km replies currentPath) $ qs

evaluateQuestion :: Int -> Int -> Int -> KnowledgeModel -> [ReplyTuple] -> String -> Question -> Int
evaluateQuestion found notFound qtnLevel km replies path q' =
  let currentPath = composePathUuid path $ q' ^. uuid'
   in case getReply replies currentPath of
        Just reply -> children currentPath
        Nothing -> isRequiredNow (q' ^. requiredLevel') qtnLevel notFound
  where
    children currentPath =
      case q' of
        MultiChoiceQuestion' q -> evaluateMultiChoiceQuestion q found notFound qtnLevel km replies currentPath
        ValueQuestion' q -> rFound
        IntegrationQuestion' q -> rFound
        OptionsQuestion' q -> rFound + evaluateOptionsQuestion q found notFound qtnLevel km replies currentPath
        ListQuestion' q -> evaluateListQuestion found notFound qtnLevel km replies currentPath q
      where
        rFound = isRequiredNow (q' ^. requiredLevel') qtnLevel found

evaluateOptionsQuestion :: OptionsQuestion -> Int -> Int -> Int -> KnowledgeModel -> [ReplyTuple] -> String -> Int
evaluateOptionsQuestion q found notFound qtnLevel km replies path =
  case getReply replies path of
    Just (_, Reply {_replyValue = AnswerReply {..}}) ->
      let currentPath = composePathUuid path _answerReplyValue
          qs = getQuestionsForAnswerUuid km _answerReplyValue
       in sum . fmap (evaluateQuestion found notFound qtnLevel km replies currentPath) $ qs
    Nothing -> isRequiredNow (q ^. requiredLevel) qtnLevel notFound

evaluateListQuestion :: Int -> Int -> Int -> KnowledgeModel -> [ReplyTuple] -> String -> ListQuestion -> Int
evaluateListQuestion found notFound qtnLevel km replies currentPath q =
  let itemQs = getItemTemplateQuestionsForQuestionUuid km $ q ^. uuid
      items =
        case getReply replies currentPath of
          Just (_, Reply {_replyValue = ItemListReply {..}}) -> _itemListReplyValue
          _ -> []
      evaluateQuestion' item =
        fmap (evaluateQuestion found notFound qtnLevel km replies (composePath currentPath $ U.toString item)) itemQs
      current =
        if not (null items)
          then isRequiredNow (q ^. requiredLevel) qtnLevel found
          else isRequiredNow (q ^. requiredLevel) qtnLevel notFound
      childrens = sum . concatMap evaluateQuestion' $ items
   in current + childrens

evaluateMultiChoiceQuestion ::
     MultiChoiceQuestion -> Int -> Int -> Int -> KnowledgeModel -> [ReplyTuple] -> String -> Int
evaluateMultiChoiceQuestion q found notFound qtnLevel km replies path =
  case getReply replies path of
    Just (_, Reply {_replyValue = MultiChoiceReply {..}}) ->
      if not (null _multiChoiceReplyValue)
        then isRequiredNow (q ^. requiredLevel) qtnLevel found
        else isRequiredNow (q ^. requiredLevel) qtnLevel notFound
    Nothing -> isRequiredNow (q ^. requiredLevel) qtnLevel notFound
