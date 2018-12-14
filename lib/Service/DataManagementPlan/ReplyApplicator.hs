module Service.DataManagementPlan.ReplyApplicator
  ( runReplyApplicator
  ) where

import Control.Lens ((&), (.~), (^.), element)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.UUID as U
import Text.Read

import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.KnowledgeModel.KnowledgeModel
import Model.Questionnaire.Questionnaire
import Service.DataManagementPlan.Convertor
import Util.List

runReplyApplicator :: FilledKnowledgeModel -> [QuestionnaireReply] -> FilledKnowledgeModel
runReplyApplicator = foldl foldReply
  where
    foldReply :: FilledKnowledgeModel -> QuestionnaireReply -> FilledKnowledgeModel
    foldReply fKM reply =
      let pathParsed = getPathParsed (reply ^. path)
      in applyReplyToKM reply pathParsed fKM

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO KNOWLEDGE MODEL
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyReplyToKM :: QuestionnaireReply -> [String] -> FilledKnowledgeModel -> FilledKnowledgeModel
applyReplyToKM reply (fChUuid:restOfPath) fKM =
  let mfChapters = foldl foldOneChapter [] (fKM ^. chapters)
  in fKM & chapters .~ mfChapters
  where
    foldOneChapter :: [FilledChapter] -> FilledChapter -> [FilledChapter]
    foldOneChapter fChs fChapter =
      if (U.toString $ fChapter ^. uuid) == fChUuid
        then fChs ++ [applyReplyToChapter reply restOfPath fChapter]
        else fChs ++ [fChapter]

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO CHAPTER
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyReplyToChapter :: QuestionnaireReply -> [String] -> FilledChapter -> FilledChapter
applyReplyToChapter reply (fQUuid:restOfPath) fCh =
  let mfQuestions = foldl foldOneQuestion [] (fCh ^. questions)
  in fCh & questions .~ mfQuestions
  where
    foldOneQuestion :: [FilledQuestion] -> FilledQuestion -> [FilledQuestion]
    foldOneQuestion fQs fQuestion =
      if (U.toString $ fQuestion ^. uuid) == fQUuid
        then fQs ++ [applyReplyToQuestion reply restOfPath fQuestion]
        else fQs ++ [fQuestion]

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO QUESTION
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyReplyToQuestion :: QuestionnaireReply -> [String] -> FilledQuestion -> FilledQuestion
applyReplyToQuestion reply [] fQuestion = replyOnQuestion reply fQuestion
applyReplyToQuestion reply (maybeItemNumber:restOfPath) fQuestion =
  case (readMaybe maybeItemNumber :: Maybe Int) of
    Just itemNumber -> passToAnswerItems reply itemNumber restOfPath fQuestion
    Nothing -> passToAnswerOption reply restOfPath fQuestion

replyOnQuestion :: QuestionnaireReply -> FilledQuestion -> FilledQuestion
replyOnQuestion reply fQuestion =
  case fQuestion ^. qType of
    QuestionTypeString -> createFilledAnswerValue fQuestion reply
    QuestionTypeNumber -> createFilledAnswerValue fQuestion reply
    QuestionTypeDate -> createFilledAnswerValue fQuestion reply
    QuestionTypeText -> createFilledAnswerValue fQuestion reply
    QuestionTypeOptions -> createFilledAnswerOption fQuestion reply
    QuestionTypeList -> createFilledAnswerItem fQuestion reply

-- -------------------------
-- CREATE REPLY ------------
-- -------------------------
createFilledAnswerValue :: FilledQuestion -> QuestionnaireReply -> FilledQuestion
createFilledAnswerValue fQuestion reply = fQuestion & answerValue .~ Just (reply ^. value)

createFilledAnswerOption :: FilledQuestion -> QuestionnaireReply -> FilledQuestion
createFilledAnswerOption fQuestion reply = fQuestion & answerOption .~ mFilledAnswer
  where
    mFilledAnswer :: Maybe FilledAnswer
    mFilledAnswer = toFilledAnswer <$> getAnswerByUuid fQuestion (reply ^. value)
    getAnswerByUuid :: FilledQuestion -> String -> Maybe Answer
    getAnswerByUuid fQuestion ansUuidS =
      case U.fromString ansUuidS of
        Just ansUuid -> find (\ans -> ans ^. uuid == ansUuid) (fromMaybe [] (fQuestion ^. answers))
        Nothing -> Nothing

createFilledAnswerItem :: FilledQuestion -> QuestionnaireReply -> FilledQuestion
createFilledAnswerItem fQuestion reply =
  case fQuestion ^. answerItemTemplate of
    Just ait ->
      let mAis = Just $ (\_ -> toFilledAnswerItem ait) <$> generateListS (reply ^. value)
      in fQuestion & answerItems .~ mAis
    Nothing -> fQuestion

-- -------------------------
-- PASS TO ANSWER ITEMS ----
-- -------------------------
applyToAnswerItem :: QuestionnaireReply -> [String] -> FilledAnswerItem -> FilledAnswerItem
applyToAnswerItem reply ("itemName":[]) ai = ai & value .~ (Just $ reply ^. value)
applyToAnswerItem reply (fQUuid:restOfPath) ai =
  let mfQuestions = foldl foldOneQuestion [] (ai ^. questions)
  in ai & questions .~ mfQuestions
  where
    foldOneQuestion :: [FilledQuestion] -> FilledQuestion -> [FilledQuestion]
    foldOneQuestion fQs fQuestion =
      if (U.toString $ fQuestion ^. uuid) == fQUuid
        then fQs ++ [applyReplyToQuestion reply restOfPath fQuestion]
        else fQs ++ [fQuestion]

passToAnswerItems :: QuestionnaireReply -> Int -> [String] -> FilledQuestion -> FilledQuestion
passToAnswerItems reply itemNumber path fQuestion =
  if length ais <= itemNumber
    then fQuestion -- Maybe return error
    else fQuestion & answerItems .~ Just mAis
  where
    ais = fromMaybe [] (fQuestion ^. answerItems)
    mAis = ais & element itemNumber .~ mAi
    mAi = applyToAnswerItem reply path (ais !! itemNumber)

-- -------------------------
-- PASS TO ANSWER OPTIONS --
-- -------------------------
applyToAnswerOption :: QuestionnaireReply -> [String] -> FilledAnswer -> FilledAnswer
applyToAnswerOption reply (fQUuid:restOfPath) fAnswer =
  let mfQuestions = foldl foldOneQuestion [] (fAnswer ^. followUps)
  in fAnswer & followUps .~ mfQuestions
  where
    foldOneQuestion :: [FilledQuestion] -> FilledQuestion -> [FilledQuestion]
    foldOneQuestion fQs fQuestion =
      if (U.toString $ fQuestion ^. uuid) == fQUuid
        then fQs ++ [applyReplyToQuestion reply restOfPath fQuestion]
        else fQs ++ [fQuestion]

passToAnswerOption :: QuestionnaireReply -> [String] -> FilledQuestion -> FilledQuestion
passToAnswerOption reply path fQuestion =
  case fQuestion ^. answerOption of
    Just answer ->
      let mAnswer = applyToAnswerOption reply path answer
      in fQuestion & answerOption .~ Just mAnswer
    Nothing -> fQuestion

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- UTILS
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
getPathParsed :: String -> [String]
getPathParsed path = T.unpack <$> (T.splitOn "." (T.pack path))
