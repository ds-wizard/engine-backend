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
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireFileDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Document.DocumentContextJM ()
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Service.Document.Context.DocumentContextMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.Tenant.TenantHelper
import qualified Wizard.Service.User.UserMapper as USR_Mapper
import WizardLib.Public.Database.DAO.User.UserGroupDAO
import WizardLib.Public.Model.User.UserGroup
import qualified WizardLib.Public.Service.User.Group.UserGroupMapper as UGR_Mapper

createDocumentContext :: Document -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> Questionnaire -> Maybe (M.Map String Reply) -> AppContextM DocumentContext
createDocumentContext doc pkg kmEditorEvents qtn mReplies = do
  km <- compileKnowledgeModelWithCaching' kmEditorEvents (Just qtn.knowledgeModelPackageId) qtn.selectedQuestionTagUuids (not . null $ kmEditorEvents)
  mQtnCreatedBy <- forM qtn.creatorUuid findUserByUuid
  mDocCreatedBy <- forM doc.createdBy findUserByUuid
  tcOrganization <- findTenantConfigOrganization
  clientUrl <- getClientUrl
  now <- liftIO getCurrentTime
  (phaseUuid, replies, labels) <-
    case mReplies of
      Just replies -> return (Nothing, replies, M.empty)
      _ -> do
        qtnEvents <- findQuestionnaireEventsByQuestionnaireUuid qtn.uuid
        let filteredQtnEvents =
              case doc.questionnaireEventUuid of
                Just eventUuid -> takeWhileInclusive (\e -> getUuid e /= eventUuid) qtnEvents
                Nothing -> qtnEvents
        qtnCtn <- compileQuestionnairePreview filteredQtnEvents
        return (qtnCtn.phaseUuid, qtnCtn.replies, qtnCtn.labels)
  report <- generateReport phaseUuid km replies
  mQtnVersion <-
    case doc.questionnaireEventUuid of
      (Just eventUuid) -> findQuestionnaireVersionByEventUuid' qtn.uuid eventUuid
      _ -> return Nothing
  qtnVersionsList <- findQuestionnaireVersionListByQuestionnaireUuidAndCreatedAt qtn.uuid (fmap (.createdAt) mQtnVersion)
  qtnFiles <- findQuestionnaireFilesSimpleByQuestionnaire doc.questionnaireUuid
  (users, groups) <- heSettingsToPerms qtn
  return $
    toDocumentContext
      doc
      clientUrl
      qtn
      phaseUuid
      replies
      labels
      mQtnVersion
      qtnVersionsList
      qtnFiles
      km
      report
      pkg
      tcOrganization
      mQtnCreatedBy
      mDocCreatedBy
      users
      groups

-- --------------------------------
-- PRIVATE
-- --------------------------------
findQuestionnaireVersionUuid :: U.UUID -> [QuestionnaireVersion] -> Maybe U.UUID
findQuestionnaireVersionUuid _ [] = Nothing
findQuestionnaireVersionUuid desiredEventUuid (version : rest)
  | desiredEventUuid == version.eventUuid = Just $ version.uuid
  | otherwise = findQuestionnaireVersionUuid desiredEventUuid rest

heSettingsToPerms :: Questionnaire -> AppContextM ([DocumentContextUserPerm], [DocumentContextUserGroupPerm])
heSettingsToPerms qtn = do
  perms <- traverse heToDocumentContextPerm qtn.permissions
  return $ partitionEithers perms

heToDocumentContextPerm :: QuestionnairePerm -> AppContextM (Either DocumentContextUserPerm DocumentContextUserGroupPerm)
heToDocumentContextPerm perm =
  case perm.memberType of
    UserQuestionnairePermType -> do
      user <- findUserByUuid perm.memberUuid
      return . Left $
        DocumentContextUserPerm
          { user = USR_Mapper.toDTO user
          , perms = perm.perms
          }
    UserGroupQuestionnairePermType -> do
      userGroup <- findUserGroupByUuid perm.memberUuid
      members <- findUsersByUserGroupUuid userGroup.uuid
      return . Right $
        DocumentContextUserGroupPerm
          { group = UGR_Mapper.toDetailDTO userGroup members
          , perms = perm.perms
          }
