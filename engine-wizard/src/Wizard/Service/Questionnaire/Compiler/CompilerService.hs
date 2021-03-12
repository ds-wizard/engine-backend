module Wizard.Service.Questionnaire.Compiler.CompilerService where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireContentDM
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Service.Cache.QuestionnaireContentCache
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

compileQuestionnaire :: Questionnaire -> AppContextM QuestionnaireContent
compileQuestionnaire qtn = do
  mQtnCtn <- getFromCache (qtn ^. events)
  case mQtnCtn of
    Just qtnCtn -> return qtnCtn
    Nothing -> do
      qtnCtn <- foldl applyEvent (return defaultQuestionnaireContent) (qtn ^. events)
      addToCache (qtn ^. events) qtnCtn
      return qtnCtn

compileQuestionnairePreview :: [QuestionnaireEvent] -> AppContextM QuestionnaireContent
compileQuestionnairePreview qtnEvents = do
  mQtnCtn <- getFromCache qtnEvents
  case mQtnCtn of
    Just qtnCtn -> return qtnCtn
    Nothing -> do
      qtnCtn <- foldl applyEvent (return defaultQuestionnaireContent) qtnEvents
      addToCache qtnEvents qtnCtn
      return qtnCtn

applyEvent :: AppContextM QuestionnaireContent -> QuestionnaireEvent -> AppContextM QuestionnaireContent
applyEvent qtnCtn' (SetReplyEvent' event) = do
  qtnCtn <- qtnCtn'
  mUser <- getUser (event ^. createdBy)
  let newReplies = M.insert (event ^. path) (toReply event mUser) (qtnCtn ^. replies)
  return $ qtnCtn & replies .~ newReplies
applyEvent qtnCtn' (ClearReplyEvent' event) = do
  qtnCtn <- qtnCtn'
  let newReplies = M.delete (event ^. path) (qtnCtn ^. replies)
  return $ qtnCtn & replies .~ newReplies
applyEvent qtnCtn' (SetLevelEvent' event) = do
  qtnCtn <- qtnCtn'
  let newLevel = event ^. level
  return $ qtnCtn & level .~ newLevel
applyEvent qtnCtn' (SetLabelsEvent' event) = do
  qtnCtn <- qtnCtn'
  let newLabels =
        case event ^. value of
          [] -> M.delete (event ^. path) (qtnCtn ^. labels)
          newValue -> M.insert (event ^. path) newValue (qtnCtn ^. labels)
  return $ qtnCtn & labels .~ newLabels

getUser mUserUuid
  -- TODO return from cache
 =
  case mUserUuid of
    Just userUuid -> Just <$> findUserById (U.toString userUuid)
    Nothing -> return Nothing

saveQuestionnaireEvent :: String -> QuestionnaireEventDTO -> AppContextM ()
saveQuestionnaireEvent qtnUuid event = do
  qtn <- findQuestionnaireById qtnUuid
  let updatedQtn = qtn & events .~ ((qtn ^. events) ++ [fromEventDTO event])
  updateQuestionnaireById updatedQtn
