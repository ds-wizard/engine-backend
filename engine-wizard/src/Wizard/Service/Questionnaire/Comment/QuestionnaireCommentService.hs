module Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService where

import Control.Lens ((.~), (^.))
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentUtils

getQuestionnaireComments :: Questionnaire -> AppContextM (M.Map String [QuestionnaireCommentThreadDTO])
getQuestionnaireComments qtn = do
  threads <- findQuestionnaireCommentThreads (U.toString $ qtn ^. uuid)
  filteredThreads <- filterComments qtn threads
  threadsDto <- traverse enhanceQuestionnaireCommentThread filteredThreads
  let commentThreadsMap = toCommentThreadsMap threadsDto
  return commentThreadsMap

duplicateCommentThreads :: String -> U.UUID -> AppContextM ()
duplicateCommentThreads oldQtnUuid newQtnUuid = do
  threads <- findQuestionnaireCommentThreads oldQtnUuid
  traverse_ (duplicateCommentThread newQtnUuid) threads

duplicateCommentThread :: U.UUID -> QuestionnaireCommentThread -> AppContextM ()
duplicateCommentThread newQtnUuid thread = do
  newUuid <- liftIO generateUuid
  let updatedCommentThread = (uuid .~ newUuid) . (questionnaireUuid .~ newQtnUuid) $ thread
  insertQuestionnaireCommentThread updatedCommentThread
  traverse_ (duplicateComment newUuid) (thread ^. comments)

duplicateComment :: U.UUID -> QuestionnaireComment -> AppContextM ()
duplicateComment newThreadUuid comment = do
  newUuid <- liftIO generateUuid
  let updatedComment = (uuid .~ newUuid) . (threadUuid .~ newThreadUuid) $ comment
  insertQuestionnaireComment updatedComment
  return ()
