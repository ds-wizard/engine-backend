module Service.Report.Evaluator.Indication
  ( computeIndications
  ) where

import Control.Lens ((^.))
import qualified Data.List as L
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors
import Model.KnowledgeModel.KnowledgeModelLenses
import Model.Questionnaire.QuestionnaireReply
import Model.Report.Report
import Util.List (generateList)

computeIndications :: Int -> KnowledgeModel -> [Reply] -> Chapter -> [Indication]
computeIndications requiredLevel km replies ch = [computeAnsweredIndication requiredLevel km replies ch]

-- --------------------------------
-- PRIVATE
-- --------------------------------
computeAnsweredIndication :: Int -> KnowledgeModel -> [Reply] -> Chapter -> Indication
computeAnsweredIndication requiredLevel km replies ch =
  AnsweredIndication' $
  AnsweredIndication
  { _answeredIndicationAnsweredQuestions = evaluateChapter 1 0 requiredLevel km replies ch
  , _answeredIndicationUnansweredQuestions = evaluateChapter 0 1 requiredLevel km replies ch
  }

evaluateChapter :: Int -> Int -> Int -> KnowledgeModel -> [Reply] -> Chapter -> Int
evaluateChapter found notFound requiredLevel km replies ch =
  let currentPath = U.toString $ ch ^. uuid
      qs = getQuestionsForChapterUuid km (ch ^. uuid)
  in sum . fmap (evaluateQuestion found notFound requiredLevel km replies currentPath) $ qs

evaluateQuestion :: Int -> Int -> Int -> KnowledgeModel -> [Reply] -> String -> Question -> Int
evaluateQuestion found notFound requiredLevel km replies path q' =
  let currentPath = composePathUuid path $ q' ^. uuid'
  in case getReply replies currentPath of
       Just reply -> children currentPath
       Nothing -> notFound
  where
    children currentPath =
      case q' of
        ValueQuestion' q -> found
        IntegrationQuestion' q -> found
        OptionsQuestion' q -> found + (evaluateOptionsQuestion found notFound requiredLevel km replies currentPath)
        ListQuestion' q -> evaluateListQuestion found notFound requiredLevel km replies currentPath q

evaluateOptionsQuestion :: Int -> Int -> Int -> KnowledgeModel -> [Reply] -> String -> Int
evaluateOptionsQuestion found notFound requiredLevel km replies path =
  case getReply replies path of
    Just (Reply {_replyValue = AnswerReply {..}}) ->
      let currentPath = composePathUuid path _answerReplyValue
          qs = getQuestionsForAnswerUuid km _answerReplyValue
      in sum . fmap (evaluateQuestion found notFound requiredLevel km replies currentPath) $ qs
    Nothing -> notFound

evaluateListQuestion :: Int -> Int -> Int -> KnowledgeModel -> [Reply] -> String -> ListQuestion -> Int
evaluateListQuestion found notFound requiredLevel km replies currentPath q =
  let itemQs = getItemTemplateQuestionsForQuestionUuid km $ q ^. uuid
      itemCount =
        case getReply replies currentPath of
          Just (Reply {_replyValue = ItemListReply {..}}) -> _itemListReplyValue
          _ -> 0
      indexes = generateList itemCount
      evaluateQuestion' index =
        fmap (evaluateQuestion found notFound requiredLevel km replies (composePath currentPath $ show index)) $ itemQs
      current =
        if itemCount > 0
          then found
          else notFound
      childrens = sum . concat . fmap evaluateQuestion' $ indexes
  in current + childrens

-- ------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------
getReply :: [Reply] -> String -> Maybe Reply
getReply replies p = L.find (\r -> r ^. path == p) replies

composePath :: String -> String -> String
composePath path element = path ++ "." ++ element

composePathUuid :: String -> U.UUID -> String
composePathUuid path uuid = composePath path (U.toString uuid)
