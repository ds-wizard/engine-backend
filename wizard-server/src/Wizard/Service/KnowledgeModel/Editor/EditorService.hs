module Wizard.Service.KnowledgeModel.Editor.EditorService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Localization.Messages.Public
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Wizard.Service.KnowledgeModel.Editor.EditorMapper
import Wizard.Service.KnowledgeModel.Editor.EditorUtil
import Wizard.Service.KnowledgeModel.Editor.EditorValidation
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Tenant.Limit.LimitService

getEditorsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page KnowledgeModelEditorList)
getEditorsPage mQuery pageable sort = do
  checkPermission _KM_PERM
  findKnowledgeModelEditorsPage mQuery pageable sort

getEditorSuggestionsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page KnowledgeModelEditorSuggestion)
getEditorSuggestionsPage mQuery pageable sort = do
  checkPermission _KM_PERM
  findKnowledgeModelEditorSuggestionsPage mQuery pageable sort

createEditor :: KnowledgeModelEditorCreateDTO -> AppContextM KnowledgeModelEditorList
createEditor reqDto =
  runInTransaction $ do
    bUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    currentUser <- getCurrentUser
    createEditorWithParams bUuid now currentUser reqDto

createEditorWithParams :: U.UUID -> UTCTime -> UserDTO -> KnowledgeModelEditorCreateDTO -> AppContextM KnowledgeModelEditorList
createEditorWithParams uuid now currentUser reqDto =
  runInTransaction $ do
    checkKnowledgeModelEditorLimit
    checkPermission _KM_PERM
    validateCreateDto reqDto
    tenantUuid <- asks currentTenantUuid
    mPreviousPkg <-
      case reqDto.previousPackageId of
        Just previousPackageId -> do
          previousPkg <- findPackageById previousPackageId
          when
            previousPkg.nonEditable
            (throwError . UserError $ _ERROR_SERVICE_PKG__NON_EDITABLE_PKG)
          return . Just $ previousPkg
        Nothing -> return Nothing
    let editor = fromCreateDTO reqDto uuid mPreviousPkg currentUser.uuid tenantUuid now
    insertKnowledgeModelEditor editor
    createDefaultEventIfPreviousPackageIsNotPresent editor
    return $ toList editor Nothing DefaultKnowledgeModelEditorState
  where
    createDefaultEventIfPreviousPackageIsNotPresent editor = do
      let mPreviousPackageId = editor.previousPackageId
      case mPreviousPackageId of
        Just _ -> return ()
        Nothing -> do
          uuid <- liftIO generateUuid
          kmUuid <- liftIO generateUuid
          let addKMEvent =
                KnowledgeModelEditorEvent
                  { uuid = uuid
                  , parentUuid = U.nil
                  , entityUuid = kmUuid
                  , content = AddKnowledgeModelEvent' AddKnowledgeModelEvent {annotations = []}
                  , knowledgeModelEditorUuid = editor.uuid
                  , tenantUuid = editor.tenantUuid
                  , createdAt = now
                  }
          void $ insertKnowledgeModelEvent addKMEvent

getEditorByUuid :: U.UUID -> AppContextM KnowledgeModelEditorDetailDTO
getEditorByUuid kmEditorUuid = do
  checkPermission _KM_PERM
  editor <- findKnowledgeModelEditorByUuid kmEditorUuid
  editorEvents <- findKnowledgeModelEventsByEditorUuid kmEditorUuid
  editorReplies <- findKnowledgeModelRepliesByEditorUuid kmEditorUuid
  mForkOfPackageId <- getEditorForkOfPackageId editor
  kmEditorState <- getEditorState editor (length editorEvents) mForkOfPackageId
  knowledgeModel <- compileKnowledgeModel [] editor.previousPackageId []
  mForkOfPackage <-
    case mForkOfPackageId of
      Just pkgId -> do
        pkg <- findPackageById pkgId
        return . Just $ pkg
      Nothing -> return Nothing
  return $ toDetailDTO editor editorEvents editorReplies knowledgeModel mForkOfPackageId mForkOfPackage kmEditorState

modifyEditor :: U.UUID -> KnowledgeModelEditorChangeDTO -> AppContextM KnowledgeModelEditorDetailDTO
modifyEditor kmEditorUuid reqDto =
  runInTransaction $ do
    checkPermission _KM_PERM
    editorFromDB <- findKnowledgeModelEditorByUuid kmEditorUuid
    validateChangeDto reqDto
    now <- liftIO getCurrentTime
    let editor =
          fromChangeDTO
            reqDto
            editorFromDB
            now
    updateKnowledgeModelEditorByUuid editor
    mForkOfPackageId <- getEditorForkOfPackageId editor
    editorEvents <- findKnowledgeModelEventsByEditorUuid kmEditorUuid
    editorReplies <- findKnowledgeModelRepliesByEditorUuid kmEditorUuid
    let kmEvents = fmap toKnowledgeModelEvent editorEvents
    kmEditorState <- getEditorState editor (length editorEvents) mForkOfPackageId
    knowledgeModel <- compileKnowledgeModel kmEvents editor.previousPackageId []
    mForkOfPackage <-
      case mForkOfPackageId of
        Just pkgId -> do
          pkg <- findPackageById pkgId
          return . Just $ pkg
        Nothing -> return Nothing
    return $ toDetailDTO editor editorEvents editorReplies knowledgeModel mForkOfPackageId mForkOfPackage kmEditorState

deleteEditor :: U.UUID -> AppContextM ()
deleteEditor kmEditorUuid =
  runInTransaction $ do
    checkPermission _KM_PERM
    _ <- findKnowledgeModelEditorByUuid kmEditorUuid
    unsetKnowledgeModelEditorFromDocumentTemplate kmEditorUuid
    deleteMigratorStateByEditorUuid kmEditorUuid
    deleteKnowledgeModelEditorByUuid kmEditorUuid
    logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged kmEditorUuid
    return ()
