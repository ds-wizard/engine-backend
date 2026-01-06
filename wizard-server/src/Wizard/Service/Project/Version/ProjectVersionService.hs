module Wizard.Service.Project.Version.ProjectVersionService where

import Control.Monad (void, when)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.List
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Project.ProjectContentDTO
import Wizard.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Wizard.Api.Resource.Project.Version.ProjectVersionRevertDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectFileDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.Event.ProjectEventListLenses ()
import Wizard.Model.Project.Project
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Model.Project.Version.ProjectVersionList
import Wizard.Service.Project.Collaboration.ProjectCollaborationService
import Wizard.Service.Project.Comment.ProjectCommentService
import Wizard.Service.Project.Compiler.ProjectCompilerService
import Wizard.Service.Project.ProjectAcl
import Wizard.Service.Project.ProjectMapper
import Wizard.Service.Project.Version.ProjectVersionMapper
import Wizard.Service.Project.Version.ProjectVersionValidation
import Wizard.Service.User.UserService

getVersions :: U.UUID -> AppContextM [ProjectVersionList]
getVersions projectUuid = do
  project <- findProjectByUuid projectUuid
  checkViewPermissionToProject project.visibility project.sharing project.permissions
  findProjectVersionListByProjectUuidAndCreatedAt projectUuid Nothing

createVersion :: U.UUID -> ProjectVersionChangeDTO -> AppContextM ProjectVersionList
createVersion projectUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkOwnerPermissionToProject project.visibility project.permissions
    validateProjectVersionCreate projectUuid reqDto
    uuid <- liftIO generateUuid
    tenantUuid <- asks currentTenantUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    let version = fromVersionChangeDTO reqDto uuid projectUuid tenantUuid currentUser.uuid now
    insertProjectVersion version
    return $ toVersionList version (Just currentUser)

cloneProjectVersions :: U.UUID -> U.UUID -> [(U.UUID, ProjectEventList)] -> AppContextM [(ProjectVersion, ProjectVersion)]
cloneProjectVersions oldProjectUuid newProjectUuid newProjectEventsWithOldEventUuid = do
  runInTransaction $ do
    oldVersions <- findProjectVersionsByProjectUuid oldProjectUuid
    traverse
      ( \oldVersion -> do
          newVersionUuid <- liftIO generateUuid
          let newEventUuid =
                case L.find (\(oldEventUuid, newEvent) -> oldVersion.eventUuid == oldEventUuid) newProjectEventsWithOldEventUuid of
                  Just (_, newEvent) -> getUuid newEvent
                  Nothing -> oldVersion.eventUuid
          let newVersion = oldVersion {uuid = newVersionUuid, projectUuid = newProjectUuid, eventUuid = newEventUuid}
          insertProjectVersion newVersion
          return (oldVersion, newVersion)
      )
      oldVersions

modifyVersion :: U.UUID -> U.UUID -> ProjectVersionChangeDTO -> AppContextM ProjectVersionList
modifyVersion projectUuid versionUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkOwnerPermissionToProject project.visibility project.permissions
    validateProjectVersionUpdate reqDto
    now <- liftIO getCurrentTime
    version <- findProjectVersionByUuid versionUuid
    let updatedVersion = fromVersionChangeDTO' version reqDto now
    updateProjectVersionByUuid updatedVersion
    createdBy <-
      case version.createdBy of
        Just vCreatedBy -> do
          user <- getUserById vCreatedBy
          return . Just $ user
        Nothing -> return Nothing
    return $ toVersionList updatedVersion createdBy

deleteVersion :: U.UUID -> U.UUID -> AppContextM ()
deleteVersion projectUuid vUuid =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkOwnerPermissionToProject project.visibility project.permissions
    _ <- findProjectVersionByUuid vUuid
    void $ deleteProjectVersionByUuid vUuid

revertToEvent :: U.UUID -> ProjectVersionRevertDTO -> Bool -> AppContextM ProjectContentDTO
revertToEvent projectUuid reqDto shouldSave =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    if shouldSave
      then checkOwnerPermissionToProject project.visibility project.permissions
      else checkViewPermissionToProject project.visibility project.sharing project.permissions
    projectVersions <- findProjectVersionsByProjectUuid projectUuid
    projectEvents <- findProjectEventListsByProjectUuid projectUuid
    let updatedEvents = takeWhileInclusive (\e -> getUuid e /= reqDto.eventUuid) projectEvents
    let eventsToDelete = dropWhileExclusive (\e -> getUuid e /= reqDto.eventUuid) projectEvents
    let updatedEventUuids = S.fromList . fmap getUuid $ updatedEvents
    let updatedVersions = filter (\v -> S.member v.eventUuid updatedEventUuids) projectVersions
    when
      shouldSave
      ( do
          let versionsToDelete = fmap (.uuid) . filter (\v -> not $ S.member v.eventUuid updatedEventUuids) $ projectVersions
          deleteProjectVersionsByUuids versionsToDelete
          deleteProjectEventsByUuids (fmap getUuid eventsToDelete)
          event <- findProjectEventByUuid reqDto.eventUuid
          deleteProjectFilesNewerThen projectUuid (getCreatedAt event)
          void $ updateProjectUpdatedAtByUuid projectUuid
      )
    let projectContent = compileProjectEvents updatedEvents
    versionDto <-
      traverse
        ( \version -> do
            createdBy <-
              case version.createdBy of
                Just vCreatedBy -> do
                  user <- getUserById vCreatedBy
                  return . Just $ user
                Nothing -> return Nothing
            return $ toVersionList version createdBy
        )
        updatedVersions
    when shouldSave (logOutOnlineUsersWhenProjectDramaticallyChanged projectUuid)
    commentThreadsMap <- catchError (getProjectCommentsByProjectUuid projectUuid Nothing Nothing) (\_ -> return M.empty)
    return $ toContentDTO projectContent commentThreadsMap updatedEvents versionDto
