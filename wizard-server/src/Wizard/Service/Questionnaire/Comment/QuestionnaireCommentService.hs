module Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService where

import Control.Monad.Except (catchError)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Service.Acl.AclService
import Shared.Common.Util.List
import Shared.Common.Util.Uuid
import Wizard.Constant.Acl
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireCommentList
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadAssigned
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadNotification
import Wizard.Service.Mail.Mailer
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper
import Wizard.Service.Questionnaire.QuestionnaireAcl
import WizardLib.Public.Model.User.UserSimple

getQuestionnaireCommentThreadsPage :: Maybe String -> Maybe U.UUID -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireCommentThreadAssigned)
getQuestionnaireCommentThreadsPage mQuery mQuestionnaireUuid resolved pageable sort = do
  checkPermission _QTN_PERM
  findAssignedQuestionnaireCommentThreadsPage mQuery mQuestionnaireUuid resolved pageable sort

getQuestionnaireCommentsByQuestionnaireUuid :: U.UUID -> Maybe String -> Maybe Bool -> AppContextM (M.Map String [QuestionnaireCommentThreadList])
getQuestionnaireCommentsByQuestionnaireUuid qtnUuid mPath mResolved = do
  qtn <- findQuestionnaireByUuid qtnUuid
  checkCommentPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  editor <- catchError (hasEditPermissionToQtn qtn.visibility qtn.sharing qtn.permissions) (\_ -> return False)
  threads <- findQuestionnaireCommentThreadsForQuestionnaire qtn.uuid mPath mResolved editor
  return . toCommentThreadsMap $ threads

duplicateCommentThreads :: U.UUID -> U.UUID -> AppContextM ()
duplicateCommentThreads oldQtnUuid newQtnUuid = do
  threads <- findQuestionnaireCommentThreads oldQtnUuid
  traverse_ (duplicateCommentThread newQtnUuid) threads

duplicateCommentThread :: U.UUID -> QuestionnaireCommentThread -> AppContextM ()
duplicateCommentThread newQtnUuid thread = do
  newUuid <- liftIO generateUuid
  let updatedCommentThread =
        thread
          { uuid = newUuid
          , questionnaireUuid = newQtnUuid
          }
  insertQuestionnaireCommentThread updatedCommentThread
  traverse_ (duplicateComment newUuid) thread.comments

duplicateComment :: U.UUID -> QuestionnaireComment -> AppContextM ()
duplicateComment newThreadUuid comment = do
  newUuid <- liftIO generateUuid
  let updatedComment =
        comment
          { uuid = newUuid
          , threadUuid = newThreadUuid
          }
  insertQuestionnaireComment updatedComment
  return ()

sendNotificationToNewAssignees :: AppContextM ()
sendNotificationToNewAssignees =
  runInTransaction $ do
    threads <- findQuestionnaireCommentThreadsForNotifying
    let threadGroups = groupBy (\t1 t2 -> t1.assignedTo.uuid == t2.assignedTo.uuid && t1.tenantUuid == t2.tenantUuid) threads
    traverse_ sendQuestionnaireCommentThreadAssignedMail threadGroups
    unsetQuestionnaireCommentThreadNotificationRequired
