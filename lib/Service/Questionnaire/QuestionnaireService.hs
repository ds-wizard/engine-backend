module Service.Questionnaire.QuestionnaireService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Database.DAO.Migration.Questionnaire.MigratorDAO
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Context.AppContextHelpers
import Model.Error.Error
import Model.Package.Package
import Model.Questionnaire.Questionnaire
import Model.Questionnaire.QuestionnaireState
import Service.KnowledgeModel.KnowledgeModelService
import Service.Package.PackageService
import Service.Questionnaire.QuestionnaireMapper
import Util.Helper
import Util.Uuid

getQuestionnaires :: AppContextM (Either AppError [QuestionnaireDTO])
getQuestionnaires =
  heFindQuestionnaires $ \questionnaires -> do
    let ioEitherQuestionnairesWithPackages = addPackage <$> questionnaires
    Prelude.foldl foldFn (return . Right $ []) ioEitherQuestionnairesWithPackages
  where
    addPackage :: Questionnaire -> AppContextM (Either AppError (Questionnaire, Package))
    addPackage qtn = heFindPackageById (qtn ^. packageId) $ \pkg -> return . Right $ (qtn, pkg)
    foldFn ::
         AppContextM (Either AppError [QuestionnaireDTO])
      -> AppContextM (Either AppError (Questionnaire, Package))
      -> AppContextM (Either AppError [QuestionnaireDTO])
    foldFn ioEitherAcc ioEitherQtnWithPkg = do
      eitherAcc <- ioEitherAcc
      case eitherAcc of
        Right acc -> do
          eitherQtnWithPkg <- ioEitherQtnWithPkg
          case eitherQtnWithPkg of
            Right (qtn, pkg) -> do
              let qtnUuid = U.toString $ qtn ^. uuid
              let pkgId = pkg ^. pId
              heGetQuestionnaireState qtnUuid pkgId $ \state -> do
                let qtnDTO = toDTO qtn pkg state
                return . Right $ acc ++ [qtnDTO]
            Left error -> return . Left $ error
        Left error -> return . Left $ error

getQuestionnairesForCurrentUser :: AppContextM (Either AppError [QuestionnaireDTO])
getQuestionnairesForCurrentUser =
  heGetCurrentUser $ \currentUser ->
    heGetQuestionnaires $ \questionnaires ->
      if currentUser ^. role == "ADMIN"
        then return . Right $ questionnaires
        else return . Right $ filter (justOwnersAndPublicQuestionnaires currentUser) questionnaires
  where
    justOwnersAndPublicQuestionnaires currentUser questionnaire =
      questionnaire ^. accessibility == PublicQuestionnaire || questionnaire ^. accessibility ==
      PublicReadOnlyQuestionnaire ||
      questionnaire ^.
      ownerUuid ==
      (Just $ currentUser ^. uuid)

createQuestionnaire :: QuestionnaireCreateDTO -> AppContextM (Either AppError QuestionnaireDTO)
createQuestionnaire questionnaireCreateDto = do
  qtnUuid <- liftIO generateUuid
  createQuestionnaireWithGivenUuid qtnUuid questionnaireCreateDto

createQuestionnaireWithGivenUuid :: U.UUID -> QuestionnaireCreateDTO -> AppContextM (Either AppError QuestionnaireDTO)
createQuestionnaireWithGivenUuid qtnUuid reqDto =
  heGetCurrentUser $ \currentUser ->
    heFindPackageWithEventsById (reqDto ^. packageId) $ \package ->
      heGetQuestionnaireState (U.toString qtnUuid) (reqDto ^. packageId) $ \qtnState -> do
        now <- liftIO getCurrentTime
        accessibility <- extractAccessibility reqDto
        let qtn = fromQuestionnaireCreateDTO reqDto qtnUuid accessibility (currentUser ^. uuid) now now
        insertQuestionnaire qtn
        return . Right $ toSimpleDTO qtn package qtnState

getQuestionnaireById :: String -> AppContextM (Either AppError QuestionnaireDTO)
getQuestionnaireById qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heCheckPermissionToQtn qtn $ heFindPackageById (qtn ^. packageId) $ \package ->
      heGetQuestionnaireState qtnUuid (package ^. pId) $ \state -> return . Right $ toDTO qtn package state

getQuestionnaireDetailById :: String -> AppContextM (Either AppError QuestionnaireDetailDTO)
getQuestionnaireDetailById qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heCheckPermissionToQtn qtn $ do
      heFindPackageWithEventsById (qtn ^. packageId) $ \package ->
        heCompileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids) $ \knowledgeModel ->
          heGetQuestionnaireState qtnUuid (package ^. pId) $ \state ->
            return . Right $ toDetailWithPackageWithEventsDTO qtn package knowledgeModel state

modifyQuestionnaire :: String -> QuestionnaireChangeDTO -> AppContextM (Either AppError QuestionnaireDetailDTO)
modifyQuestionnaire qtnUuid reqDto =
  heGetQuestionnaireDetailById qtnUuid $ \qtnDto ->
    heCheckEditPermissionToQtn qtnDto $ heGetCurrentUser $ \currentUser -> do
      now <- liftIO getCurrentTime
      accessibility <- extractAccessibility reqDto
      let updatedQtn = fromChangeDTO qtnDto reqDto accessibility (currentUser ^. uuid) now
      let pkgId = qtnDto ^. package ^. pId
      updateQuestionnaireById updatedQtn
      heCompileKnowledgeModel [] (Just pkgId) (updatedQtn ^. selectedTagUuids) $ \knowledgeModel ->
        heGetQuestionnaireState qtnUuid pkgId $ \state ->
          return . Right $ toDetailWithPackageDTO updatedQtn (qtnDto ^. package) knowledgeModel state

deleteQuestionnaire :: String -> AppContextM (Maybe AppError)
deleteQuestionnaire qtnUuid =
  hmGetQuestionnaireById qtnUuid $ \qtn ->
    hmCheckEditPermissionToQtn qtn $ do
      deleteQuestionnaireById qtnUuid
      deleteMigratorStateByNewQuestionnaireId qtnUuid
      return Nothing

-- --------------------------------
-- PRIVATE
-- --------------------------------
extractAccessibility dto = do
  dswConfig <- asks _appContextAppConfig
  if dswConfig ^. general . questionnaireAccessibilityEnabled
    then return (dto ^. accessibility)
    else return PrivateQuestionnaire

heCheckPermissionToQtn qtn callback =
  heGetCurrentUser $ \currentUser ->
    if currentUser ^. role == "ADMIN" || qtn ^. accessibility == PublicQuestionnaire || qtn ^. accessibility ==
       PublicReadOnlyQuestionnaire ||
       qtn ^.
       ownerUuid ==
       (Just $ currentUser ^. uuid)
      then callback
      else return . Left . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Get Questionnaire"

-- -----------------------------------------------------
heCheckEditPermissionToQtn qtn callback =
  heGetCurrentUser $ \currentUser ->
    if currentUser ^. role == "ADMIN" || qtn ^. accessibility == PublicQuestionnaire || qtn ^. ownerUuid ==
       (Just $ currentUser ^. uuid)
      then callback
      else return . Left . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"

hmCheckEditPermissionToQtn qtn callback =
  hmGetCurrentUser $ \currentUser ->
    if currentUser ^. role == "ADMIN" || qtn ^. accessibility == PublicQuestionnaire || qtn ^. ownerUuid ==
       (Just $ currentUser ^. uuid)
      then callback
      else return . Just . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"

-- -----------------------------------------------------
heCheckMigrationPermissionToQtn qtn callback =
  heGetCurrentUser $ \currentUser ->
    if currentUser ^. role == "ADMIN" || qtn ^. accessibility == PublicQuestionnaire || qtn ^. ownerUuid ==
       (Just $ currentUser ^. uuid)
      then callback
      else return . Left . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Questionnaire"

-- -----------------------------------------------------
getQuestionnaireState :: String -> String -> AppContextM (Either AppError QuestionnaireState)
getQuestionnaireState qtnUuid pkgId = do
  eMigrationState <- findMigratorStateByNewQuestionnaireId qtnUuid
  case eMigrationState of
    Right _ -> return . Right $ QSMigrating
    Left (NotExistsError _) ->
      heGetNewerPackages pkgId $ \pkgs ->
        if Prelude.length pkgs == 0
          then return . Right $ QSDefault
          else return . Right $ QSOutdated
    Left error -> return . Left $ error

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetQuestionnaires callback = do
  eitherQuestionnaires <- getQuestionnaires
  case eitherQuestionnaires of
    Right questionnaires -> callback questionnaires
    Left error -> return . Left $ error

-- -----------------------------------------------------
heGetQuestionnaireById qtnUuid callback = do
  eitherQuestionnaire <- getQuestionnaireById qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Left $ error

hmGetQuestionnaireById qtnUuid callback = do
  eitherQuestionnaire <- getQuestionnaireById qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Just $ error

-- -----------------------------------------------------
heGetQuestionnaireDetailById qtnUuid callback = do
  eitherQuestionnaire <- getQuestionnaireDetailById qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Left $ error

-- -----------------------------------------------------
hmDeleteQuestionnaire qtnUuid callback = do
  mError <- deleteQuestionnaire qtnUuid
  case mError of
    Nothing -> callback
    Just error -> return . Just $ error

-- -----------------------------------------------------
heGetQuestionnaireState qtnUuid pkgId = createHeeHelper (getQuestionnaireState qtnUuid pkgId)
