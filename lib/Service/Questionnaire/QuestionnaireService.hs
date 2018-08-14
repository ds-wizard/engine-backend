module Service.Questionnaire.QuestionnaireService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Package.Package
import Model.Questionnaire.Questionnaire
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Package.PackageService
import Service.Questionnaire.QuestionnaireMapper
import Util.Uuid

getQuestionnaires :: AppContextM (Either AppError [QuestionnaireDTO])
getQuestionnaires = do
  eitherQuestionnaires <- findQuestionnaires
  liftIO $ print eitherQuestionnaires
  case eitherQuestionnaires of
    Right questionnaires -> do
      let ioEitherQuestionnairesWithPackages = addPackage <$> questionnaires
      Prelude.foldl foldFn (return . Right $ []) ioEitherQuestionnairesWithPackages
    Left error -> return . Left $ error
  where
    addPackage :: Questionnaire -> AppContextM (Either AppError (Questionnaire, Package))
    addPackage qtn = do
      eitherPackage <- findPackageById (qtn ^. packageId)
      case eitherPackage of
        Right package -> return . Right $ (qtn, package)
        Left error -> return . Left $ error
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
              let qtnDTO = toDTO qtn pkg
              return . Right $ acc ++ [qtnDTO]
            Left error -> return . Left $ error
        Left error -> return . Left $ error

createQuestionnaire :: QuestionnaireCreateDTO -> AppContextM (Either AppError QuestionnaireDTO)
createQuestionnaire questionnaireCreateDto = do
  qtnUuid <- liftIO generateUuid
  createQuestionnaireWithGivenUuid qtnUuid questionnaireCreateDto

createQuestionnaireWithGivenUuid :: U.UUID -> QuestionnaireCreateDTO -> AppContextM (Either AppError QuestionnaireDTO)
createQuestionnaireWithGivenUuid qtnUuid reqDto =
  heFindPackageWithEventsById (reqDto ^. packageId) $ \package ->
    getEvents (reqDto ^. packageId) $ \events ->
      getKnowledgeModel events $ \knowledgeModel -> do
        now <- liftIO getCurrentTime
        let qtn = fromQuestionnaireCreateDTO reqDto qtnUuid knowledgeModel now now
        insertQuestionnaire qtn
        return . Right $ toSimpleDTO qtn package
  where
    getEvents pkgId callback = do
      eitherEvents <- getAllPreviousEventsSincePackageId pkgId
      case eitherEvents of
        Right events -> callback events
        Left error -> return . Left $ error
    getKnowledgeModel events callback = do
      let eitherKm = createKnowledgeModel $ events
      case eitherKm of
        Right km -> callback km
        Left error -> return . Left $ error

getQuestionnaireById :: String -> AppContextM (Either AppError QuestionnaireDTO)
getQuestionnaireById qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heFindPackageById (qtn ^. packageId) $ \package -> return . Right $ toDTO qtn package

getQuestionnaireDetailById :: String -> AppContextM (Either AppError QuestionnaireDetailDTO)
getQuestionnaireDetailById qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heFindPackageWithEventsById (qtn ^. packageId) $ \package ->
      return . Right $ toDetailWithPackageWithEventsDTO qtn package

modifyQuestionnaire :: String -> QuestionnaireChangeDTO -> AppContextM (Either AppError QuestionnaireDetailDTO)
modifyQuestionnaire qtnUuid reqDto =
  heGetQuestionnaireDetailById qtnUuid $ \qtnDto -> do
    now <- liftIO getCurrentTime
    let updateQtn = fromChangeDTO qtnDto reqDto now
    updateQuestionnaireById updateQtn
    return . Right $ toDetailWithPackageDTODTO updateQtn (qtnDto ^. package)

deleteQuestionnaire :: String -> AppContextM (Maybe AppError)
deleteQuestionnaire qtnUuid = do
  eitherQuestionnaire <- findQuestionnaireById qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> do
      deleteQuestionnaireById qtnUuid
      return Nothing
    Left error -> return . Just $ error

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetQuestionnaireById qtnUuid callback = do
  eitherQuestionnaire <- getQuestionnaireById qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Left $ error

heGetQuestionnaireDetailById qtnUuid callback = do
  eitherQuestionnaire <- getQuestionnaireDetailById qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Left $ error
