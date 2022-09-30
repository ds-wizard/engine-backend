module Wizard.Service.Questionnaire.QuestionnaireCommandExecutor where

import Control.Lens ((^.))
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireSquash
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper
import Wizard.Service.Questionnaire.QuestionnaireUtils
import Wizard.Util.Logger

cComponent = "Questionnaire"

execute :: PersistentCommand -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command ^. function == cMoveQuestionnaireCommentsToSeparateTableName =
    cMoveQuestionnaireCommentsToSeparateTable command

cMoveQuestionnaireCommentsToSeparateTableName = "moveQuestionnaireCommentsToSeparateTable"

cMoveQuestionnaireCommentsToSeparateTable :: PersistentCommand -> AppContextM (PersistentCommandState, Maybe String)
cMoveQuestionnaireCommentsToSeparateTable persistentCommand = do
  qtnUuids <- findQuestionnaireUuids
  traverse_ moveForOneQuestionnaire qtnUuids
  return (DonePersistentCommandState, Nothing)

moveForOneQuestionnaire :: U.UUID -> AppContextM ()
moveForOneQuestionnaire qtnUuid =
  runInTransaction $ do
    logInfoU _CMP_SERVICE (f' "Moving comments to separate table forquestionnaire (qtnUuid: '%s')" [U.toString qtnUuid])
    (QuestionnaireSquash _ events versions) <- findQuestionnaireSquashById (U.toString qtnUuid)
    traverse_ (processEvent qtnUuid) events
    let filteredEvents = filter excludeQuestionnaireCommentEvent events
    updateQuestionnaireEventsByUuid'' (U.toString qtnUuid) filteredEvents
    logInfoU
      _CMP_SERVICE
      (f' "Moving comments to separate table for questionnaire '%s' finished successfully" [U.toString qtnUuid])

processEvent :: U.UUID -> QuestionnaireEvent -> AppContextM ()
processEvent _ (ResolveCommentThreadEvent' event) = do
  updateQuestionnaireCommentThreadResolvedById (event ^. threadUuid) True
  return ()
processEvent _ (ReopenCommentThreadEvent' event) = do
  updateQuestionnaireCommentThreadResolvedById (event ^. threadUuid) False
  return ()
processEvent _ (DeleteCommentThreadEvent' event) = do
  deleteQuestionnaireCommentThreadById (event ^. threadUuid)
  return ()
processEvent qtnUuid (AddCommentEvent' event) = do
  let comment = toComment' event
  mThread <- findQuestionnaireCommentThreadById (U.toString $ event ^. threadUuid)
  case mThread of
    Just _ -> insertQuestionnaireComment comment
    Nothing -> do
      let thread = toCommentThread' event qtnUuid
      insertQuestionnaireThreadAndComment' thread comment
  return ()
processEvent _ (EditCommentEvent' event) = do
  updateQuestionnaireCommentTextById (event ^. commentUuid) (event ^. text)
  return ()
processEvent _ (DeleteCommentEvent' event) = do
  deleteQuestionnaireCommentById (event ^. commentUuid)
  return ()
processEvent _ _ = return ()
