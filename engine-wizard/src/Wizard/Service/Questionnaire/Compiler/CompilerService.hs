module Wizard.Service.Questionnaire.Compiler.CompilerService where

import Control.Lens ((&), (.~), (^.), (^?), _Just)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.UUID as U

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireContentDM
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Service.Cache.QuestionnaireContentCache
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

compileQuestionnaire :: HasEvents s [QuestionnaireEvent] => s -> AppContextM QuestionnaireContent
compileQuestionnaire qtn =
  runInTransaction $ do
    mQtnCtn <- getFromCache (qtn ^. events)
    case mQtnCtn of
      Just qtnCtn -> return qtnCtn
      Nothing -> do
        qtnCtn <- foldl applyEvent (return defaultQuestionnaireContent) (qtn ^. events)
        addToCache (qtn ^. events) qtnCtn
        return qtnCtn

compileQuestionnairePreview :: [QuestionnaireEvent] -> AppContextM QuestionnaireContent
compileQuestionnairePreview qtnEvents =
  runInTransaction $ do
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
applyEvent qtnCtn' (ResolveCommentThreadEvent' event) = do
  qtnCtn <- qtnCtn'
  mUser <- getUser (event ^. createdBy)
  let updateThread t =
        if t ^. uuid == event ^. threadUuid
          then (updatedAt .~ (event ^. createdAt)) . (resolved .~ True) $ t
          else t
  let threads =
        case M.lookup (event ^. path) (qtnCtn ^. commentThreadsMap) of
          Nothing -> []
          Just [] -> []
          Just threads -> fmap updateThread threads
  let newCommentThreadsMap = M.insert (event ^. path) threads (qtnCtn ^. commentThreadsMap)
  return $ qtnCtn & commentThreadsMap .~ newCommentThreadsMap
applyEvent qtnCtn' (ReopenCommentThreadEvent' event) = do
  qtnCtn <- qtnCtn'
  mUser <- getUser (event ^. createdBy)
  let updateThread t =
        if t ^. uuid == event ^. threadUuid
          then (updatedAt .~ (event ^. createdAt)) . (resolved .~ False) $ t
          else t
  let threads =
        case M.lookup (event ^. path) (qtnCtn ^. commentThreadsMap) of
          Nothing -> []
          Just [] -> []
          Just threads -> fmap updateThread threads
  let newCommentThreadsMap = M.insert (event ^. path) threads (qtnCtn ^. commentThreadsMap)
  return $ qtnCtn & commentThreadsMap .~ newCommentThreadsMap
applyEvent qtnCtn' (DeleteCommentThreadEvent' event) = do
  qtnCtn <- qtnCtn'
  mUser <- getUser (event ^. createdBy)
  let filterFn t =
        not
          ((t ^. uuid == event ^. threadUuid) && isJust (event ^. createdBy) &&
           ((t ^. createdBy ^? _Just . uuid) == event ^. createdBy))
  let threads =
        case M.lookup (event ^. path) (qtnCtn ^. commentThreadsMap) of
          Nothing -> []
          Just [] -> []
          Just threads -> filter filterFn threads
  let newCommentThreadsMap = M.insert (event ^. path) threads (qtnCtn ^. commentThreadsMap)
  return $ qtnCtn & commentThreadsMap .~ newCommentThreadsMap
applyEvent qtnCtn' (AddCommentEvent' event) = do
  qtnCtn <- qtnCtn'
  mUser <- getUser (event ^. createdBy)
  let updateThread t =
        if t ^. uuid == event ^. threadUuid
          then (updatedAt .~ (event ^. createdAt)) . (comments .~ ((t ^. comments) ++ [toComment event mUser])) $ t
          else t
  let threads =
        case M.lookup (event ^. path) (qtnCtn ^. commentThreadsMap) of
          Nothing -> [toCommentThread event mUser]
          Just [] -> [toCommentThread event mUser]
          Just threads ->
            if isJust $ L.find (\t -> t ^. uuid == event ^. threadUuid) threads
              then fmap updateThread threads
              else threads ++ [toCommentThread event mUser]
  let newCommentThreadsMap = M.insert (event ^. path) threads (qtnCtn ^. commentThreadsMap)
  return $ qtnCtn & commentThreadsMap .~ newCommentThreadsMap
applyEvent qtnCtn' (EditCommentEvent' event) = do
  qtnCtn <- qtnCtn'
  mUser <- getUser (event ^. createdBy)
  let updateComment c =
        if (c ^. uuid == event ^. commentUuid) && isJust (event ^. createdBy) &&
           ((c ^. createdBy ^? _Just . uuid) == event ^. createdBy)
          then (updatedAt .~ (event ^. createdAt)) . (text .~ (event ^. text)) $ c
          else c
  let updateThread t =
        if t ^. uuid == event ^. threadUuid
          then (updatedAt .~ (event ^. createdAt)) . (comments .~ fmap updateComment (t ^. comments)) $ t
          else t
  let threads =
        case M.lookup (event ^. path) (qtnCtn ^. commentThreadsMap) of
          Nothing -> []
          Just [] -> []
          Just threads -> fmap updateThread threads
  let newCommentThreadsMap = M.insert (event ^. path) threads (qtnCtn ^. commentThreadsMap)
  return $ qtnCtn & commentThreadsMap .~ newCommentThreadsMap
applyEvent qtnCtn' (DeleteCommentEvent' event) = do
  qtnCtn <- qtnCtn'
  mUser <- getUser (event ^. createdBy)
  let filterFn c =
        not
          ((c ^. uuid == event ^. commentUuid) && isJust (event ^. createdBy) &&
           ((c ^. createdBy ^? _Just . uuid) == event ^. createdBy))
  let updateThread t =
        if t ^. uuid == event ^. threadUuid
          then (updatedAt .~ (event ^. createdAt)) . (comments .~ filter filterFn (t ^. comments)) $ t
          else t
  let threads =
        case M.lookup (event ^. path) (qtnCtn ^. commentThreadsMap) of
          Nothing -> []
          Just [] -> []
          Just threads -> fmap updateThread threads
  let newCommentThreadsMap = M.insert (event ^. path) threads (qtnCtn ^. commentThreadsMap)
  return $ qtnCtn & commentThreadsMap .~ newCommentThreadsMap

getUser mUserUuid =
  case mUserUuid of
    Just userUuid -> findUserById' (U.toString userUuid)
    Nothing -> return Nothing
