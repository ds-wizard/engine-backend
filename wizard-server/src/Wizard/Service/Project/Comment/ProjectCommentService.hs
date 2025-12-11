module Wizard.Service.Project.Comment.ProjectCommentService where

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
import Wizard.Database.DAO.Project.ProjectCommentDAO
import Wizard.Database.DAO.Project.ProjectCommentThreadDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Comment.ProjectComment
import Wizard.Model.Project.Comment.ProjectCommentList
import Wizard.Model.Project.Comment.ProjectCommentThreadAssigned
import Wizard.Model.Project.Comment.ProjectCommentThreadNotification
import Wizard.Model.Project.Project
import Wizard.Service.Mail.Mailer
import Wizard.Service.Project.Comment.ProjectCommentMapper
import Wizard.Service.Project.ProjectAcl
import WizardLib.Public.Model.User.UserSimple

getProjectCommentThreadsPage :: Maybe String -> Maybe U.UUID -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page ProjectCommentThreadAssigned)
getProjectCommentThreadsPage mQuery mProjectUuid resolved pageable sort = do
  checkPermission _PRJ_PERM
  findAssignedProjectCommentThreadsPage mQuery mProjectUuid resolved pageable sort

getProjectCommentsByProjectUuid :: U.UUID -> Maybe String -> Maybe Bool -> AppContextM (M.Map String [ProjectCommentThreadList])
getProjectCommentsByProjectUuid projectUuid mPath mResolved = do
  project <- findProjectByUuid projectUuid
  checkCommentPermissionToProject project.visibility project.sharing project.permissions
  editor <- catchError (hasEditPermissionToProject project.visibility project.sharing project.permissions) (\_ -> return False)
  threads <- findProjectCommentThreadsForProject project.uuid mPath mResolved editor
  return . toCommentThreadsMap $ threads

duplicateCommentThreads :: U.UUID -> U.UUID -> AppContextM ()
duplicateCommentThreads oldProjectUuid newProjectUuid = do
  threads <- findProjectCommentThreads oldProjectUuid
  traverse_ (duplicateCommentThread newProjectUuid) threads

duplicateCommentThread :: U.UUID -> ProjectCommentThread -> AppContextM ()
duplicateCommentThread newProjectUuid thread = do
  newUuid <- liftIO generateUuid
  let updatedCommentThread =
        thread
          { uuid = newUuid
          , projectUuid = newProjectUuid
          }
  insertProjectCommentThread updatedCommentThread
  traverse_ (duplicateComment newUuid) thread.comments

duplicateComment :: U.UUID -> ProjectComment -> AppContextM ()
duplicateComment newThreadUuid comment = do
  newUuid <- liftIO generateUuid
  let updatedComment =
        comment
          { uuid = newUuid
          , threadUuid = newThreadUuid
          }
  insertProjectComment updatedComment
  return ()

sendNotificationToNewAssignees :: AppContextM ()
sendNotificationToNewAssignees =
  runInTransaction $ do
    threads <- findProjectCommentThreadsForNotifying
    let threadGroups = groupBy (\t1 t2 -> t1.assignedTo.uuid == t2.assignedTo.uuid && t1.tenantUuid == t2.tenantUuid) threads
    traverse_ sendProjectCommentThreadAssignedMail threadGroups
    unsetProjectCommentThreadNotificationRequired
