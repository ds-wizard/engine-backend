module Wizard.Service.Questionnaire.QuestionnaireUtils where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Except (catchError)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Database.DAO.Acl.GroupDAO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Acl.Acl
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireDetail
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Service.Cache.QuestionnaireReportCache
import Wizard.Service.Config.AppConfigService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper
import Wizard.Service.Report.ReportGenerator

extractVisibility dto = do
  appConfig <- getAppConfig
  if appConfig ^. questionnaire . questionnaireVisibility . enabled
    then return (dto ^. visibility)
    else return $ appConfig ^. questionnaire . questionnaireVisibility . defaultValue

extractSharing dto = do
  appConfig <- getAppConfig
  if appConfig ^. questionnaire . questionnaireSharing . enabled
    then return (dto ^. sharing)
    else return $ appConfig ^. questionnaire . questionnaireSharing . defaultValue

enhanceQuestionnaire :: Questionnaire -> AppContextM QuestionnaireDTO
enhanceQuestionnaire qtn = do
  pkg <- getPackageById (qtn ^. packageId)
  state <- getQuestionnaireState (U.toString $ qtn ^. uuid) (pkg ^. pId)
  report <- getQuestionnaireReport qtn
  permissionDtos <- traverse enhanceQuestionnairePermRecord (qtn ^. permissions)
  qtnCtn <- compileQuestionnaire qtn
  return $ toDTO qtn qtnCtn pkg state report permissionDtos

enhanceQuestionnaireDetail :: QuestionnaireDetail -> AppContextM QuestionnaireDTO
enhanceQuestionnaireDetail qtnDetail = do
  report <- getQuestionnaireReport qtnDetail
  qtnCtn <- compileQuestionnaire qtnDetail
  return $ toDTO' qtnDetail qtnCtn report

enhanceQuestionnairePermRecord :: QuestionnairePermRecord -> AppContextM QuestionnairePermRecordDTO
enhanceQuestionnairePermRecord record =
  case record ^. member of
    UserMember {_userMemberUuid = userUuid} -> do
      user <- findUserById (U.toString userUuid)
      return $ toUserPermRecordDTO record user
    GroupMember {_groupMemberGId = groupId} -> do
      group <- findGroupById groupId
      return $ toGroupPermRecordDTO record group

enhanceQuestionnaireEvent :: QuestionnaireEvent -> AppContextM QuestionnaireEventDTO
enhanceQuestionnaireEvent event = do
  mUser <-
    case event ^. createdBy' of
      Just userUuid -> findUserById' (U.toString userUuid)
      Nothing -> return Nothing
  return $ toEventDTO event mUser

enhanceQuestionnaireVersion :: QuestionnaireVersion -> AppContextM QuestionnaireVersionDTO
enhanceQuestionnaireVersion version = do
  mUser <- findUserById' (U.toString $ version ^. createdBy)
  return $ toVersionDTO version mUser

excludeQuestionnaireCommentEvent :: QuestionnaireEvent -> Bool
excludeQuestionnaireCommentEvent (ResolveCommentThreadEvent' _) = False
excludeQuestionnaireCommentEvent (ReopenCommentThreadEvent' _) = False
excludeQuestionnaireCommentEvent (DeleteCommentThreadEvent' _) = False
excludeQuestionnaireCommentEvent (AddCommentEvent' _) = False
excludeQuestionnaireCommentEvent (EditCommentEvent' _) = False
excludeQuestionnaireCommentEvent (DeleteCommentEvent' _) = False
excludeQuestionnaireCommentEvent _ = True

getQuestionnaireState :: String -> String -> AppContextM QuestionnaireState
getQuestionnaireState qtnUuid pkgId = do
  mMs <- findMigratorStateByNewQuestionnaireId' qtnUuid
  case mMs of
    Just _ -> return QSMigrating
    Nothing -> do
      pkgs <- getNewerPackages pkgId
      if null pkgs
        then return QSDefault
        else return QSOutdated

getQuestionnaireReport ::
     ( HasEvents questionnaire [QuestionnaireEvent]
     , HasUuid questionnaire U.UUID
     , HasPackageId questionnaire String
     , HasSelectedTagUuids questionnaire [U.UUID]
     )
  => questionnaire
  -> AppContextM QuestionnaireReportDTO
getQuestionnaireReport qtn = do
  qtnCtn <- compileQuestionnaire qtn
  mIndications <- getFromCache qtn qtnCtn
  case mIndications of
    Just indications -> return . toQuestionnaireReportDTO $ indications
    Nothing -> do
      appConfig <- getAppConfig
      let _requiredPhaseUuid = qtnCtn ^. phaseUuid
      let _replies = M.toList $ qtnCtn ^. replies
      km <- compileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids)
      let indications = computeTotalReportIndications _requiredPhaseUuid km _replies
      qtnCtn <- compileQuestionnaire qtn
      addToCache qtn qtnCtn indications
      return . toQuestionnaireReportDTO $ indications

skipIfAssigningProject :: Questionnaire -> AppContextM () -> AppContextM ()
skipIfAssigningProject qtn action = do
  appConfig <- getAppConfig
  let questionnaireSharingEnabled = appConfig ^. questionnaire . questionnaireSharing . enabled
  let questionnaireSharingAnonymousEnabled = appConfig ^. questionnaire . questionnaireSharing . anonymousEnabled
  when
    (not (questionnaireSharingEnabled && questionnaireSharingAnonymousEnabled) || (not . null $ qtn ^. permissions))
    action

filterComments ::
     Questionnaire
  -> M.Map String [QuestionnaireCommentThread]
  -> AppContextM (M.Map String [QuestionnaireCommentThread])
filterComments qtn commentThreadsMap =
  catchError
    (do checkEditPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. permissions)
        return commentThreadsMap)
    (\_ ->
       catchError
         (do checkCommentPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. permissions)
             return $ M.map (filter (\t -> not (t ^. private))) commentThreadsMap)
         (\_ -> return M.empty))
