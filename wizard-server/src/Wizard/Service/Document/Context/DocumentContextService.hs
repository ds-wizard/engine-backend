module Wizard.Service.Document.Context.DocumentContextService (
  createDocumentContext,
) where

import Control.Monad (forM)
import Control.Monad.Reader (liftIO)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.List
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectFileDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Document.DocumentContextJM ()
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Event.ProjectEventListLenses ()
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Service.Document.Context.DocumentContextMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Project.Compiler.ProjectCompilerService
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.Tenant.TenantHelper
import qualified Wizard.Service.User.UserMapper as USR_Mapper
import WizardLib.Public.Database.DAO.User.UserGroupDAO
import WizardLib.Public.Model.User.UserGroup
import qualified WizardLib.Public.Service.User.Group.UserGroupMapper as UGR_Mapper

createDocumentContext :: Document -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> Project -> Maybe (M.Map String Reply) -> AppContextM DocumentContext
createDocumentContext doc pkg kmEditorEvents project mReplies = do
  km <- compileKnowledgeModelWithCaching' kmEditorEvents (Just project.knowledgeModelPackageId) project.selectedQuestionTagUuids (not . null $ kmEditorEvents)
  mProjectCreatedBy <- forM project.creatorUuid findUserByUuid
  mDocCreatedBy <- forM doc.createdBy findUserByUuid
  tcOrganization <- findTenantConfigOrganization
  clientUrl <- getClientUrl
  now <- liftIO getCurrentTime
  (phaseUuid, replies, labels) <-
    case mReplies of
      Just replies -> return (Nothing, replies, M.empty)
      _ -> do
        projectEvents <- findProjectEventListsByProjectUuid project.uuid
        let filteredProjectEvents =
              case doc.projectEventUuid of
                Just eventUuid -> takeWhileInclusive (\e -> getUuid e /= eventUuid) projectEvents
                Nothing -> projectEvents
        let projectContent = compileProjectEvents filteredProjectEvents
        return (projectContent.phaseUuid, projectContent.replies, projectContent.labels)
  report <- generateReport phaseUuid km replies
  mProjectVersion <-
    case doc.projectEventUuid of
      (Just eventUuid) -> findProjectVersionByEventUuid' project.uuid eventUuid
      _ -> return Nothing
  projectVersionsList <- findProjectVersionListByProjectUuidAndCreatedAt project.uuid (fmap (.createdAt) mProjectVersion)
  projectFiles <-
    case doc.projectUuid of
      Just projectUuid -> findProjectFilesSimpleByProject projectUuid
      Nothing -> return []
  (users, groups) <- heSettingsToPerms project
  return $
    toDocumentContext
      doc
      clientUrl
      project
      phaseUuid
      replies
      labels
      mProjectVersion
      projectVersionsList
      projectFiles
      km
      report
      pkg
      tcOrganization
      mProjectCreatedBy
      mDocCreatedBy
      users
      groups

-- --------------------------------
-- PRIVATE
-- --------------------------------
findProjectVersionUuid :: U.UUID -> [ProjectVersion] -> Maybe U.UUID
findProjectVersionUuid _ [] = Nothing
findProjectVersionUuid desiredEventUuid (version : rest)
  | desiredEventUuid == version.eventUuid = Just $ version.uuid
  | otherwise = findProjectVersionUuid desiredEventUuid rest

heSettingsToPerms :: Project -> AppContextM ([DocumentContextUserPerm], [DocumentContextUserGroupPerm])
heSettingsToPerms project = do
  perms <- traverse heToDocumentContextPerm project.permissions
  return $ partitionEithers perms

heToDocumentContextPerm :: ProjectPerm -> AppContextM (Either DocumentContextUserPerm DocumentContextUserGroupPerm)
heToDocumentContextPerm perm =
  case perm.memberType of
    UserProjectPermType -> do
      user <- findUserByUuid perm.memberUuid
      return . Left $
        DocumentContextUserPerm
          { user = USR_Mapper.toDTO user
          , perms = perm.perms
          }
    UserGroupProjectPermType -> do
      userGroup <- findUserGroupByUuid perm.memberUuid
      members <- findUsersByUserGroupUuid userGroup.uuid
      return . Right $
        DocumentContextUserGroupPerm
          { group = UGR_Mapper.toDetailDTO userGroup members
          , perms = perm.perms
          }
