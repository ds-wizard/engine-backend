module Wizard.Service.Document.Context.DocumentContextService (
  createDocumentContext,
) where

import Control.Monad (forM)
import Control.Monad.Reader (liftIO)
import Data.Either (partitionEithers)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.List
import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireFileDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Document.DocumentContextJM ()
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Document.Context.DocumentContextMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireUtil
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.TenantHelper
import qualified Wizard.Service.User.UserMapper as USR_Mapper
import WizardLib.Public.Database.DAO.User.UserGroupDAO
import WizardLib.Public.Model.User.UserGroup
import qualified WizardLib.Public.Service.User.Group.UserGroupMapper as UGR_Mapper

createDocumentContext :: Document -> AppContextM DocumentContext
createDocumentContext doc = do
  qtn <- findQuestionnaireByUuid doc.questionnaireUuid
  qtnSettings <- findQuestionnaireDetailSettings doc.questionnaireUuid
  pkg <- getPackageById qtn.packageId
  km <- compileKnowledgeModel [] (Just qtn.packageId) qtn.selectedQuestionTagUuids
  mQtnCreatedBy <- forM qtn.creatorUuid findUserByUuid
  mDocCreatedBy <- forM doc.createdBy findUserByUuid
  tenantConfig <- getCurrentTenantConfig
  clientUrl <- getClientUrl
  let org = tenantConfig.organization
  now <- liftIO getCurrentTime
  let qtnEvents =
        case doc.questionnaireEventUuid of
          Just eventUuid -> takeWhileInclusive (\e -> getUuid e /= eventUuid) qtn.events
          Nothing -> qtn.events
  qtnCtn <- compileQuestionnairePreview qtnEvents
  report <- generateReport qtnCtn.phaseUuid km qtnCtn.replies
  let qtnVersion =
        case doc.questionnaireEventUuid of
          (Just eventUuid) -> findQuestionnaireVersionUuid eventUuid qtn.versions
          _ -> Nothing
  qtnVersionDtos <- traverse enhanceQuestionnaireVersion qtn.versions
  qtnFiles <- findQuestionnaireFilesByQuestionnaire doc.questionnaireUuid
  (users, groups) <- heSettingsToPerms qtnSettings
  return $
    toDocumentContext
      doc
      clientUrl
      qtn
      qtnCtn
      qtnVersion
      qtnVersionDtos
      qtnFiles
      km
      report
      pkg
      org
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

heSettingsToPerms :: QuestionnaireDetailSettings -> AppContextM ([DocumentContextUserPerm], [DocumentContextUserGroupPerm])
heSettingsToPerms qtnSettings = do
  perms <- traverse heToDocumentContextPerm qtnSettings.permissions
  return $ partitionEithers perms

heToDocumentContextPerm :: QuestionnairePermDTO -> AppContextM (Either DocumentContextUserPerm DocumentContextUserGroupPerm)
heToDocumentContextPerm perm@(QuestionnairePermDTO {perms = perms, member = UserMemberDTO {uuid = uuid}}) = do
  user <- findUserByUuid uuid
  return . Left $
    DocumentContextUserPerm
      { user = USR_Mapper.toDTO user
      , perms = perms
      }
heToDocumentContextPerm perm@(QuestionnairePermDTO {perms = perms, member = UserGroupMemberDTO {uuid = uuid}}) = do
  userGroup <- findUserGroupByUuid uuid
  members <- findUsersByUserGroupUuid userGroup.uuid
  return . Right $
    DocumentContextUserGroupPerm
      { group = UGR_Mapper.toDetailDTO userGroup members
      , perms = perms
      }
