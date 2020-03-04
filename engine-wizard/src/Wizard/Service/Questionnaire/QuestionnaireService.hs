module Wizard.Service.Questionnaire.QuestionnaireService where

import Control.Lens ((.~), (^.))
import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireValidation

getQuestionnaires :: AppContextM [QuestionnaireDTO]
getQuestionnaires = do
  questionnaires <- findQuestionnaires
  forM questionnaires enhance
  where
    enhance :: Questionnaire -> AppContextM QuestionnaireDTO
    enhance qtn = do
      pkg <- findPackageById (qtn ^. packageId)
      state <- getQuestionnaireState (U.toString $ qtn ^. uuid) (pkg ^. pId)
      return $ toDTO qtn pkg state

getQuestionnairesForCurrentUser :: AppContextM [QuestionnaireDTO]
getQuestionnairesForCurrentUser = do
  currentUser <- getCurrentUser
  questionnaires <- getQuestionnaires
  if currentUser ^. role == "ADMIN"
    then return questionnaires
    else return $ filter (justOwnersAndPublicQuestionnaires currentUser) questionnaires
  where
    justOwnersAndPublicQuestionnaires currentUser questionnaire =
      questionnaire ^. accessibility == PublicQuestionnaire || questionnaire ^. accessibility ==
      PublicReadOnlyQuestionnaire ||
      questionnaire ^.
      ownerUuid ==
      (Just $ currentUser ^. uuid)

createQuestionnaire :: QuestionnaireCreateDTO -> AppContextM QuestionnaireDTO
createQuestionnaire questionnaireCreateDto = do
  qtnUuid <- liftIO generateUuid
  createQuestionnaireWithGivenUuid qtnUuid questionnaireCreateDto

createQuestionnaireWithGivenUuid :: U.UUID -> QuestionnaireCreateDTO -> AppContextM QuestionnaireDTO
createQuestionnaireWithGivenUuid qtnUuid reqDto = do
  currentUser <- getCurrentUser
  package <- findPackageWithEventsById (reqDto ^. packageId)
  qtnState <- getQuestionnaireState (U.toString qtnUuid) (reqDto ^. packageId)
  now <- liftIO getCurrentTime
  accessibility <- extractAccessibility reqDto
  let qtn = fromQuestionnaireCreateDTO reqDto qtnUuid accessibility (currentUser ^. uuid) now now
  insertQuestionnaire qtn
  return $ toSimpleDTO qtn package qtnState

cloneQuestionnaire :: String -> AppContextM QuestionnaireDTO
cloneQuestionnaire cloneUuid = do
  qtnDto <- getQuestionnaireDetailById cloneUuid
  pkg <- findPackageWithEventsById (qtnDto ^. package . pId)
  newUuid <- liftIO generateUuid
  currentUser <- getCurrentUser
  let newOwnerUuid =
        if qtnDto ^. accessibility == PublicQuestionnaire
          then Nothing
          else Just $ currentUser ^. uuid
  now <- liftIO getCurrentTime
  let originQtn = fromDetailDTO qtnDto
  let newQtn =
        ownerUuid .~ newOwnerUuid $ uuid .~ newUuid $ name .~ ("Copy of " ++ originQtn ^. name) $ updatedAt .~ now $
        originQtn
  insertQuestionnaire newQtn
  state <- getQuestionnaireState (U.toString newUuid) (pkg ^. pId)
  return $ toSimpleDTO newQtn pkg state

getQuestionnaireById :: String -> AppContextM QuestionnaireDTO
getQuestionnaireById qtnUuid = do
  mQtn <- getQuestionnaireById' qtnUuid
  case mQtn of
    Just qtn -> return qtn
    Nothing -> throwError $ NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "questionnaire" qtnUuid

getQuestionnaireById' :: String -> AppContextM (Maybe QuestionnaireDTO)
getQuestionnaireById' qtnUuid = do
  mQtn <- findQuestionnaireById' qtnUuid
  case mQtn of
    Just qtn -> do
      checkPermissionToQtn qtn
      package <- findPackageById (qtn ^. packageId)
      state <- getQuestionnaireState qtnUuid (package ^. pId)
      return . Just $ toDTO qtn package state
    Nothing -> return Nothing

getQuestionnaireDetailById :: String -> AppContextM QuestionnaireDetailDTO
getQuestionnaireDetailById qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  checkPermissionToQtn qtn
  package <- findPackageWithEventsById (qtn ^. packageId)
  knowledgeModel <- compileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids)
  state <- getQuestionnaireState qtnUuid (package ^. pId)
  return $ toDetailWithPackageWithEventsDTO qtn package knowledgeModel state

modifyQuestionnaire :: String -> QuestionnaireChangeDTO -> AppContextM QuestionnaireDetailDTO
modifyQuestionnaire qtnUuid reqDto = do
  qtnDto <- getQuestionnaireDetailById qtnUuid
  checkEditPermissionToQtn qtnDto
  currentUser <- getCurrentUser
  now <- liftIO getCurrentTime
  accessibility <- extractAccessibility reqDto
  let updatedQtn = fromChangeDTO qtnDto reqDto accessibility (currentUser ^. uuid) now
  let pkgId = qtnDto ^. package . pId
  updateQuestionnaireById updatedQtn
  knowledgeModel <- compileKnowledgeModel [] (Just pkgId) (updatedQtn ^. selectedTagUuids)
  state <- getQuestionnaireState qtnUuid pkgId
  return $ toDetailWithPackageDTO updatedQtn (qtnDto ^. package) knowledgeModel state

deleteQuestionnaire :: String -> AppContextM ()
deleteQuestionnaire qtnUuid = do
  qtn <- getQuestionnaireById qtnUuid
  validateQuestionnaireDeletation qtnUuid
  checkEditPermissionToQtn qtn
  deleteQuestionnaireById qtnUuid
  deleteMigratorStateByNewQuestionnaireId qtnUuid
  return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
extractAccessibility dto = do
  appConfig <- asks _appContextApplicationConfig
  if appConfig ^. general . questionnaireAccessibilityEnabled
    then return (dto ^. accessibility)
    else return PrivateQuestionnaire

-- -----------------------------------------------------
checkPermissionToQtn qtn = do
  currentUser <- getCurrentUser
  if currentUser ^. role == "ADMIN" || qtn ^. accessibility == PublicQuestionnaire || qtn ^. accessibility ==
     PublicReadOnlyQuestionnaire ||
     qtn ^.
     ownerUuid ==
     (Just $ currentUser ^. uuid)
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Get Questionnaire"

-- -----------------------------------------------------
checkEditPermissionToQtn qtn = do
  currentUser <- getCurrentUser
  if currentUser ^. role == "ADMIN" || qtn ^. accessibility == PublicQuestionnaire || qtn ^. ownerUuid ==
     (Just $ currentUser ^. uuid)
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"

-- -----------------------------------------------------
checkMigrationPermissionToQtn qtn = do
  currentUser <- getCurrentUser
  if currentUser ^. role == "ADMIN" || qtn ^. accessibility == PublicQuestionnaire || qtn ^. ownerUuid ==
     (Just $ currentUser ^. uuid)
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Questionnaire"

-- -----------------------------------------------------
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
