module Wizard.Service.Questionnaire.QuestionnaireCommandExecutor where

import Control.Lens ((.~), (^.))
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.UUID as U

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireSquash
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService
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
  qtnUuids <- findQuestionnaireUuids'
  traverse_ moveForOneQuestionnaire qtnUuids
  return (DonePersistentCommandState, Nothing)

moveForOneQuestionnaire :: U.UUID -> AppContextM ()
moveForOneQuestionnaire qtnUuid =
  runInTransaction $ do
    logInfoU _CMP_SERVICE (f' "Moving comments to separate table forquestionnaire (qtnUuid: '%s')" [U.toString qtnUuid])
    (QuestionnaireSquash _ events versions) <- findQuestionnaireSquashById (U.toString qtnUuid)
    let threadsMap = foldl (applyEvent qtnUuid) M.empty events
    traverse_ (duplicateCommentThread qtnUuid) . M.elems $ threadsMap
    let filteredEvents = filter excludeQuestionnaireCommentEvent events
    updateQuestionnaireEventsByUuid'' (U.toString qtnUuid) filteredEvents
    logInfoU
      _CMP_SERVICE
      (f' "Moving comments to separate table for questionnaire '%s' finished successfully" [U.toString qtnUuid])

applyEvent ::
     U.UUID -> M.Map U.UUID QuestionnaireCommentThread -> QuestionnaireEvent -> M.Map U.UUID QuestionnaireCommentThread
applyEvent _ threadMaps (ResolveCommentThreadEvent' event) = do
  case M.lookup (event ^. threadUuid) threadMaps of
    Nothing -> threadMaps
    Just t ->
      let thread = (updatedAt .~ (event ^. createdAt)) . (resolved .~ True) $ t
       in M.insert (event ^. threadUuid) thread threadMaps
applyEvent _ threadMaps (ReopenCommentThreadEvent' event) = do
  case M.lookup (event ^. threadUuid) threadMaps of
    Nothing -> threadMaps
    Just t ->
      let thread = (updatedAt .~ (event ^. createdAt)) . (resolved .~ False) $ t
       in M.insert (event ^. threadUuid) thread threadMaps
applyEvent _ threadMaps (DeleteCommentThreadEvent' event) = do
  M.delete (event ^. threadUuid) threadMaps
applyEvent qtnUuid threadMaps (AddCommentEvent' event) = do
  let thread =
        case M.lookup (event ^. threadUuid) threadMaps of
          Nothing -> toCommentThread' event qtnUuid
          Just t -> (updatedAt .~ (event ^. createdAt)) . (comments .~ ((t ^. comments) ++ [toComment' event])) $ t
  M.insert (event ^. threadUuid) thread threadMaps
applyEvent _ threadMaps (EditCommentEvent' event) = do
  let updateComment c =
        if (c ^. uuid == event ^. commentUuid) && isJust (event ^. createdBy) && (c ^. createdBy == event ^. createdBy)
          then (updatedAt .~ (event ^. createdAt)) . (text .~ (event ^. text)) $ c
          else c
  case M.lookup (event ^. threadUuid) threadMaps of
    Nothing -> threadMaps
    Just t ->
      let thread = (updatedAt .~ (event ^. createdAt)) . (comments .~ fmap updateComment (t ^. comments)) $ t
       in M.insert (event ^. threadUuid) thread threadMaps
applyEvent _ threadMaps (DeleteCommentEvent' event) = do
  let filterFn c =
        not
          ((c ^. uuid == event ^. commentUuid) && isJust (event ^. createdBy) && (c ^. createdBy == event ^. createdBy))
  case M.lookup (event ^. threadUuid) threadMaps of
    Nothing -> threadMaps
    Just t ->
      let thread = (updatedAt .~ (event ^. createdAt)) . (comments .~ filter filterFn (t ^. comments)) $ t
       in M.insert (event ^. threadUuid) thread threadMaps
applyEvent _ threadMaps _ = threadMaps
