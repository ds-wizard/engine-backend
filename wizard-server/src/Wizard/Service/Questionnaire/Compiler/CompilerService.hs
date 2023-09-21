module Wizard.Service.Questionnaire.Compiler.CompilerService where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Records

import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireContentDM
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.User.User
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

compileQuestionnaire :: HasField "events" s [QuestionnaireEvent] => s -> AppContextM QuestionnaireContent
compileQuestionnaire qtn = do
  (_, qtnCtn) <- foldl applyEvent (return ([], defaultQuestionnaireContent)) qtn.events
  return qtnCtn

compileQuestionnairePreview :: [QuestionnaireEvent] -> AppContextM QuestionnaireContent
compileQuestionnairePreview events = do
  (_, qtnCtn) <- foldl applyEvent (return ([], defaultQuestionnaireContent)) events
  return qtnCtn

applyEvent :: AppContextM ([U.UUID], QuestionnaireContent) -> QuestionnaireEvent -> AppContextM ([U.UUID], QuestionnaireContent)
applyEvent context (SetReplyEvent' event) = do
  (deletedUserUuids, qtnCtn) <- context
  (updatedDeletedUserUuids, mUser) <- getUser deletedUserUuids event.createdBy
  let newReplies = M.insert event.path (toReply event mUser) qtnCtn.replies
  return (updatedDeletedUserUuids, qtnCtn {replies = newReplies})
applyEvent context (ClearReplyEvent' event) = do
  (deletedUserUuids, qtnCtn) <- context
  let newReplies = M.delete event.path qtnCtn.replies
  return (deletedUserUuids, qtnCtn {replies = newReplies})
applyEvent context (SetPhaseEvent' event) = do
  (deletedUserUuids, qtnCtn) <- context
  let newPhaseUuid = event.phaseUuid
  return (deletedUserUuids, qtnCtn {phaseUuid = newPhaseUuid})
applyEvent context (SetLabelsEvent' event) = do
  (deletedUserUuids, qtnCtn) <- context
  let newLabels =
        case event.value of
          [] -> M.delete event.path qtnCtn.labels
          newValue -> M.insert event.path newValue qtnCtn.labels
  return (deletedUserUuids, qtnCtn {labels = newLabels})

getUser :: [U.UUID] -> Maybe U.UUID -> AppContextM ([U.UUID], Maybe User)
getUser deletedUserUuids mCreatedBy =
  case mCreatedBy of
    Just createdBy -> do
      if createdBy `elem` deletedUserUuids
        then return (deletedUserUuids, Nothing)
        else do
          mUser <- findUserByUuid' createdBy
          case mUser of
            Just user -> return (deletedUserUuids, Just user)
            Nothing -> return (createdBy : deletedUserUuids, Nothing)
    Nothing -> return (deletedUserUuids, Nothing)
