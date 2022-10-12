module Wizard.Service.Questionnaire.Compiler.CompilerService where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireContentDM
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

compileQuestionnaire :: HasEvents s [QuestionnaireEvent] => s -> AppContextM QuestionnaireContent
compileQuestionnaire qtn = foldl applyEvent (return defaultQuestionnaireContent) (qtn ^. events)

compileQuestionnairePreview :: [QuestionnaireEvent] -> AppContextM QuestionnaireContent
compileQuestionnairePreview = foldl applyEvent (return defaultQuestionnaireContent)

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
applyEvent qtnCtn' (SetPhaseEvent' event) = do
  qtnCtn <- qtnCtn'
  let newPhaseUuid = event ^. phaseUuid
  return $ qtnCtn & phaseUuid .~ newPhaseUuid
applyEvent qtnCtn' (SetLabelsEvent' event) = do
  qtnCtn <- qtnCtn'
  let newLabels =
        case event ^. value of
          [] -> M.delete (event ^. path) (qtnCtn ^. labels)
          newValue -> M.insert (event ^. path) newValue (qtnCtn ^. labels)
  return $ qtnCtn & labels .~ newLabels
applyEvent qtnCtn' _ = qtnCtn'

getUser mUserUuid =
  case mUserUuid of
    Just userUuid -> findUserById' (U.toString userUuid)
    Nothing -> return Nothing
