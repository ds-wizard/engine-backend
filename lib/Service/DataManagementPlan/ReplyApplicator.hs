module Service.DataManagementPlan.ReplyApplicator
  ( runReplyApplicator
  ) where

import Control.Lens ((&), (.~), (^.), element)
import Data.List
import Data.Maybe
import qualified Data.UUID as U
import Text.Read

import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.FilledKnowledgeModel.FilledKnowledgeModelAccessors
import Model.KnowledgeModel.KnowledgeModel
import Model.Questionnaire.QuestionnaireReply
import Service.DataManagementPlan.Convertor
import Util.List
import Util.String (splitOn)

runReplyApplicator :: FilledKnowledgeModel -> [Reply] -> FilledKnowledgeModel
runReplyApplicator = foldl foldReply
  where
    foldReply :: FilledKnowledgeModel -> Reply -> FilledKnowledgeModel
    foldReply fKM reply =
      let pathParsed = splitOn "." (reply ^. path)
      in applyReplyToKM reply pathParsed fKM

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO KNOWLEDGE MODEL
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyReplyToKM :: Reply -> [String] -> FilledKnowledgeModel -> FilledKnowledgeModel
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
applyReplyToChapter :: Reply -> [String] -> FilledChapter -> FilledChapter
applyReplyToChapter reply (fQUuid:restOfPath) fCh =
  let mfQuestions = foldl foldOneQuestion [] (fCh ^. questions)
  in fCh & questions .~ mfQuestions
  where
    foldOneQuestion :: [FilledQuestion] -> FilledQuestion -> [FilledQuestion]
    foldOneQuestion fQs fQuestion =
      if (U.toString . getFilledQuestionUuid $ fQuestion) == fQUuid
        then fQs ++ [applyReplyToQuestion reply restOfPath fQuestion]
        else fQs ++ [fQuestion]

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO QUESTION
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyReplyToQuestion :: Reply -> [String] -> FilledQuestion -> FilledQuestion
applyReplyToQuestion reply [] fQuestion = replyOnQuestion reply fQuestion
applyReplyToQuestion reply (maybeItemNumber:restOfPath) fQuestion =
  case (readMaybe maybeItemNumber :: Maybe Int) of
    Just itemNumber -> passToAnswerItems reply itemNumber restOfPath fQuestion
    Nothing -> passToAnswerOption reply restOfPath fQuestion

replyOnQuestion :: Reply -> FilledQuestion -> FilledQuestion
replyOnQuestion reply (FilledOptionsQuestion' fQuestion) =
  FilledOptionsQuestion' $ createFilledAnswerOption fQuestion reply
replyOnQuestion reply (FilledListQuestion' fQuestion) = FilledListQuestion' $ createFilledAnswerItem fQuestion reply
replyOnQuestion reply (FilledValueQuestion' fQuestion) = FilledValueQuestion' $ createFilledAnswerValue fQuestion reply
replyOnQuestion reply (FilledIntegrationQuestion' fQuestion) =
  FilledIntegrationQuestion' $ createFilledAnswerIntegration fQuestion reply

-- -------------------------
-- CREATE REPLY ------------
-- -------------------------
createFilledAnswerValue :: FilledValueQuestion -> Reply -> FilledValueQuestion
createFilledAnswerValue fQuestion reply = fQuestion & answerValue .~ Just (getReplyValue $ reply ^. value)
  where
    getReplyValue :: ReplyValue -> String
    getReplyValue StringReply {..} = _stringReplyValue

createFilledAnswerOption :: FilledOptionsQuestion -> Reply -> FilledOptionsQuestion
createFilledAnswerOption fQuestion reply = fQuestion & answerOption .~ mFilledAnswer
  where
    mFilledAnswer :: Maybe FilledAnswer
    mFilledAnswer = toFilledAnswer fQuestion <$> getAnswerByUuid fQuestion (getReplyValue $ reply ^. value)
    getAnswerByUuid :: FilledOptionsQuestion -> U.UUID -> Maybe Answer
    getAnswerByUuid fQuestion ansUuid = find (\ans -> ans ^. uuid == ansUuid) (fQuestion ^. answers)
    getReplyValue :: ReplyValue -> U.UUID
    getReplyValue AnswerReply {..} = _answerReplyValue

createFilledAnswerItem :: FilledListQuestion -> Reply -> FilledListQuestion
createFilledAnswerItem fQuestion reply =
  let mAis = Just $ (\number -> toFilledAnswerItem number fQuestion) <$> generateList (getReplyValue $ reply ^. value)
  in fQuestion & items .~ mAis
  where
    getReplyValue :: ReplyValue -> Int
    getReplyValue ItemListReply {..} = _itemListReplyValue

createFilledAnswerIntegration :: FilledIntegrationQuestion -> Reply -> FilledIntegrationQuestion
createFilledAnswerIntegration fQuestion reply = applyReplyValue . applyReplyIntId $ fQuestion
  where
    applyReplyIntId :: FilledIntegrationQuestion -> FilledIntegrationQuestion
    applyReplyIntId q = q & answerIntId .~ (getReplyIntId $ reply ^. value)
      where
        getReplyIntId :: ReplyValue -> Maybe String
        getReplyIntId IntegrationReply {..} = getIntReplyIntId _integrationReplyValue
          where
            getIntReplyIntId :: IntegrationReplyValue -> Maybe String
            getIntReplyIntId (PlainValue value) = Nothing
            getIntReplyIntId IntegrationValue {..} = Just _integrationValueIntId
    applyReplyValue :: FilledIntegrationQuestion -> FilledIntegrationQuestion
    applyReplyValue q = q & answerValue .~ Just (getReplyValue $ reply ^. value)
      where
        getReplyValue :: ReplyValue -> String
        getReplyValue IntegrationReply {..} = getIntReplyValue _integrationReplyValue
          where
            getIntReplyValue :: IntegrationReplyValue -> String
            getIntReplyValue (PlainValue value) = value
            getIntReplyValue IntegrationValue {..} = _integrationValueIntValue

-- -------------------------
-- PASS TO ANSWER ITEMS ----
-- -------------------------
applyToAnswerItem :: Reply -> [String] -> FilledAnswerItem -> FilledAnswerItem
applyToAnswerItem reply ("itemName":[]) ai = ai & value .~ (Just . getReplyValue $ reply ^. value)
  where
    getReplyValue :: ReplyValue -> String
    getReplyValue StringReply {..} = _stringReplyValue
applyToAnswerItem reply (fQUuid:restOfPath) ai =
  let mfQuestions = foldl foldOneQuestion [] (ai ^. questions)
  in ai & questions .~ mfQuestions
  where
    foldOneQuestion :: [FilledQuestion] -> FilledQuestion -> [FilledQuestion]
    foldOneQuestion fQs fQuestion =
      if (U.toString . getFilledQuestionUuid $ fQuestion) == fQUuid
        then fQs ++ [applyReplyToQuestion reply restOfPath fQuestion]
        else fQs ++ [fQuestion]

passToAnswerItems :: Reply -> Int -> [String] -> FilledQuestion -> FilledQuestion
passToAnswerItems reply itemNumber path (FilledListQuestion' fQuestion) =
  if length ais <= itemNumber
    then FilledListQuestion' fQuestion -- Maybe return error
    else FilledListQuestion' $ fQuestion & items .~ Just mAis
  where
    ais = fromMaybe [] (fQuestion ^. items)
    mAis = ais & element itemNumber .~ mAi
    mAi = applyToAnswerItem reply path (ais !! itemNumber)
passToAnswerItems reply itemNumber path fQuestion = fQuestion -- Maybe return error

-- -------------------------
-- PASS TO ANSWER OPTIONS --
-- -------------------------
applyToAnswerOption :: Reply -> [String] -> FilledAnswer -> FilledAnswer
applyToAnswerOption reply (fQUuid:restOfPath) fAnswer =
  let mfQuestions = foldl foldOneQuestion [] (fAnswer ^. followUps)
  in fAnswer & followUps .~ mfQuestions
  where
    foldOneQuestion :: [FilledQuestion] -> FilledQuestion -> [FilledQuestion]
    foldOneQuestion fQs fQuestion =
      if (U.toString . getFilledQuestionUuid $ fQuestion) == fQUuid
        then fQs ++ [applyReplyToQuestion reply restOfPath fQuestion]
        else fQs ++ [fQuestion]

passToAnswerOption :: Reply -> [String] -> FilledQuestion -> FilledQuestion
passToAnswerOption reply path (FilledOptionsQuestion' fQuestion) =
  case fQuestion ^. answerOption of
    Just answer ->
      let mAnswer = applyToAnswerOption reply path answer
      in FilledOptionsQuestion' $ fQuestion & answerOption .~ Just mAnswer
    Nothing -> FilledOptionsQuestion' $ fQuestion
passToAnswerOption reply path fQuestion = fQuestion
