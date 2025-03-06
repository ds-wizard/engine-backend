module Wizard.Service.Questionnaire.Version.QuestionnaireVersionService where

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
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Questionnaire.QuestionnaireVersionList
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireUtil
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionValidation
import Wizard.Service.User.UserService

getVersions :: U.UUID -> AppContextM [QuestionnaireVersionList]
getVersions qtnUuid = do
  qtn <- findQuestionnaireByUuid qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  findQuestionnaireVersionListByQuestionnaireUuid qtnUuid

createVersion :: U.UUID -> QuestionnaireVersionChangeDTO -> AppContextM QuestionnaireVersionList
createVersion qtnUuid reqDto =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    checkOwnerPermissionToQtn qtn.visibility qtn.permissions
    validateQuestionnaireVersionCreate qtnUuid reqDto
    uuid <- liftIO generateUuid
    tenantUuid <- asks currentTenantUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    let version = fromVersionChangeDTO reqDto uuid qtnUuid tenantUuid currentUser.uuid now
    insertQuestionnaireVersion version
    return $ toVersionList version (Just currentUser)

cloneQuestionnaireVersions :: U.UUID -> U.UUID -> [(U.UUID, QuestionnaireEvent)] -> AppContextM [(QuestionnaireVersion, QuestionnaireVersion)]
cloneQuestionnaireVersions oldQtnUuid newQtnUuid newQtnEventsWithOldEventUuid = do
  runInTransaction $ do
    oldVersions <- findQuestionnaireVersionsByQuestionnaireUuid oldQtnUuid
    traverse
      ( \oldVersion -> do
          newVersionUuid <- liftIO generateUuid
          let newEvenUuid =
                case L.find (\(oldEventUuid, newEvent) -> oldVersion.eventUuid == oldEventUuid) newQtnEventsWithOldEventUuid of
                  Just (_, newEvent) -> getUuid newEvent
                  Nothing -> oldVersion.eventUuid
          let newVersion = oldVersion {uuid = newVersionUuid, questionnaireUuid = newQtnUuid, eventUuid = newEvenUuid}
          insertQuestionnaireVersion newVersion
          return (oldVersion, newVersion)
      )
      oldVersions

modifyVersion :: U.UUID -> U.UUID -> QuestionnaireVersionChangeDTO -> AppContextM QuestionnaireVersionList
modifyVersion qtnUuid versionUuid reqDto =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    checkOwnerPermissionToQtn qtn.visibility qtn.permissions
    validateQuestionnaireVersionUpdate reqDto
    now <- liftIO getCurrentTime
    version <- findQuestionnaireVersionByUuid versionUuid
    let updatedVersion = fromVersionChangeDTO' version reqDto now
    updateQuestionnaireVersionByUuid updatedVersion
    createdBy <-
      case version.createdBy of
        Just vCreatedBy -> do
          user <- getUserById vCreatedBy
          return . Just $ user
        Nothing -> return Nothing
    return $ toVersionList updatedVersion createdBy

deleteVersion :: U.UUID -> U.UUID -> AppContextM ()
deleteVersion qtnUuid vUuid =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    checkOwnerPermissionToQtn qtn.visibility qtn.permissions
    _ <- findQuestionnaireVersionByUuid vUuid
    void $ deleteQuestionnaireVersionByUuid vUuid

revertToEvent :: U.UUID -> QuestionnaireVersionRevertDTO -> Bool -> AppContextM QuestionnaireContentDTO
revertToEvent qtnUuid reqDto shouldSave =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    if shouldSave
      then checkOwnerPermissionToQtn qtn.visibility qtn.permissions
      else checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
    qtnVersions <- findQuestionnaireVersionsByQuestionnaireUuid qtnUuid
    qtnEvents <- findQuestionnaireEventsByQuestionnaireUuid qtnUuid
    let updatedEvents = takeWhileInclusive (\e -> getUuid e /= reqDto.eventUuid) qtnEvents
    let eventsToDelete = dropWhileExclusive (\e -> getUuid e /= reqDto.eventUuid) qtnEvents
    let updatedEventUuids = S.fromList . fmap getUuid $ updatedEvents
    let updatedVersions = filter (\v -> S.member v.eventUuid updatedEventUuids) qtnVersions
    when
      shouldSave
      ( do
          let versionsToDelete = fmap (.uuid) . filter (\v -> not $ S.member v.eventUuid updatedEventUuids) $ qtnVersions
          deleteQuestionnaireVersionsByUuids versionsToDelete
          deleteQuestionnaireEventsByUuids (fmap getUuid eventsToDelete)
      )
    qtnCtn <- compileQuestionnaire updatedEvents
    eventsDto <- traverse enhanceQuestionnaireEvent updatedEvents
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
    when shouldSave (logOutOnlineUsersWhenQtnDramaticallyChanged qtnUuid)
    commentThreadsMap <- catchError (getQuestionnaireCommentsByQuestionnaireUuid qtnUuid Nothing Nothing) (\_ -> return M.empty)
    return $ toContentDTO qtnCtn commentThreadsMap eventsDto versionDto
