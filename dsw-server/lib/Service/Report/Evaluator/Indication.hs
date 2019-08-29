module Service.Report.Evaluator.Indication
  ( computeIndications
  ) where

import Control.Lens ((^.))
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors
import Model.KnowledgeModel.KnowledgeModelLenses
import Model.Questionnaire.QuestionnaireReply
import Model.Report.Report
import Util.List (generateList)

computeIndications :: Bool -> Int -> KnowledgeModel -> [Reply] -> Chapter -> [Indication]
computeIndications levelsEnabled qtnLevel km replies ch =
  if levelsEnabled
    then [computeLevelsAnsweredIndication qtnLevel km replies ch, computeAnsweredIndication 9999 km replies ch]
    else [computeAnsweredIndication 9999 km replies ch]

-- --------------------------------
-- PRIVATE
-- --------------------------------
computeAnsweredIndication :: Int -> KnowledgeModel -> [Reply] -> Chapter -> Indication
computeAnsweredIndication qtnLevel km replies ch =
  AnsweredIndication' $
  AnsweredIndication
  { _answeredIndicationAnsweredQuestions = evaluateChapter 1 0 qtnLevel km replies ch
  , _answeredIndicationUnansweredQuestions = evaluateChapter 0 1 qtnLevel km replies ch
  }

computeLevelsAnsweredIndication :: Int -> KnowledgeModel -> [Reply] -> Chapter -> Indication
computeLevelsAnsweredIndication qtnLevel km replies ch =
  LevelsAnsweredIndication' $
  LevelsAnsweredIndication
  { _levelsAnsweredIndicationAnsweredQuestions = evaluateChapter 1 0 qtnLevel km replies ch
  , _levelsAnsweredIndicationUnansweredQuestions = evaluateChapter 0 1 qtnLevel km replies ch
  }

evaluateChapter :: Int -> Int -> Int -> KnowledgeModel -> [Reply] -> Chapter -> Int
evaluateChapter found notFound qtnLevel km replies ch =
  let currentPath = U.toString $ ch ^. uuid
      qs = getQuestionsForChapterUuid km (ch ^. uuid)
  in sum . fmap (evaluateQuestion found notFound qtnLevel km replies currentPath) $ qs

evaluateQuestion :: Int -> Int -> Int -> KnowledgeModel -> [Reply] -> String -> Question -> Int
evaluateQuestion found notFound qtnLevel km replies path q' =
  let currentPath = composePathUuid path $ q' ^. uuid'
  in case getReply replies currentPath of
       Just reply -> children currentPath
       Nothing -> isRequiredNow (q' ^. requiredLevel') qtnLevel notFound
  where
    children currentPath =
      case q' of
        ValueQuestion' q -> rFound
        IntegrationQuestion' q -> rFound
        OptionsQuestion' q -> rFound + (evaluateOptionsQuestion q found notFound qtnLevel km replies currentPath)
        ListQuestion' q -> evaluateListQuestion found notFound qtnLevel km replies currentPath q
      where
        rFound = isRequiredNow (q' ^. requiredLevel') qtnLevel found

evaluateOptionsQuestion :: OptionsQuestion -> Int -> Int -> Int -> KnowledgeModel -> [Reply] -> String -> Int
evaluateOptionsQuestion q found notFound qtnLevel km replies path =
  case getReply replies path of
    Just (Reply {_replyValue = AnswerReply {..}}) ->
      let currentPath = composePathUuid path _answerReplyValue
          qs = getQuestionsForAnswerUuid km _answerReplyValue
      in sum . fmap (evaluateQuestion found notFound qtnLevel km replies currentPath) $ qs
    Nothing -> isRequiredNow (q ^. requiredLevel) qtnLevel notFound

evaluateListQuestion :: Int -> Int -> Int -> KnowledgeModel -> [Reply] -> String -> ListQuestion -> Int
evaluateListQuestion found notFound qtnLevel km replies currentPath q =
  let itemQs = getItemTemplateQuestionsForQuestionUuid km $ q ^. uuid
      itemCount =
        case getReply replies currentPath of
          Just (Reply {_replyValue = ItemListReply {..}}) -> _itemListReplyValue
          _ -> 0
      indexes = generateList itemCount
      evaluateQuestion' index =
        fmap (evaluateQuestion found notFound qtnLevel km replies (composePath currentPath $ show index)) $ itemQs
      current =
        if itemCount > 0
          then isRequiredNow (q ^. requiredLevel) qtnLevel found
          else isRequiredNow (q ^. requiredLevel) qtnLevel notFound
      childrens = sum . concat . fmap evaluateQuestion' $ indexes
  in current + childrens

-- ------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------
getReply :: [Reply] -> String -> Maybe Reply
getReply replies p = L.find (\r -> r ^. path == p) replies

isRequiredNow :: Maybe Int -> Int -> Int -> Int
isRequiredNow mQLevel qtnLevel currentValue =
  if qtnLevel == 9999
    then currentValue
    else let qLevel = fromMaybe 0 mQLevel
         in if qLevel <= qtnLevel
              then currentValue
              else 0

composePath :: String -> String -> String
composePath path element = path ++ "." ++ element

composePathUuid :: String -> U.UUID -> String
composePathUuid path uuid = composePath path (U.toString uuid)
