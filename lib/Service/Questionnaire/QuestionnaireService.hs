module Service.Questionnaire.QuestionnaireService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Api.Resource.User.UserDTO
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

getQuestionnairesForCurrentUser :: UserDTO -> AppContextM (Either AppError [QuestionnaireDTO])
getQuestionnairesForCurrentUser userDto =
  heGetQuestionnaires $ \questionnaires ->
    if userDto ^. role == "ADMIN"
      then return . Right $ questionnaires
      else return . Right $ filter justOwnersAndPublicQuestionnaires questionnaires
  where
    justOwnersAndPublicQuestionnaires questionnaire =
      questionnaire ^. private == False || questionnaire ^. ownerUuid == (Just $ userDto ^. uuid)

createQuestionnaire :: UserDTO -> QuestionnaireCreateDTO -> AppContextM (Either AppError QuestionnaireDTO)
createQuestionnaire userDto questionnaireCreateDto = do
  qtnUuid <- liftIO generateUuid
  createQuestionnaireWithGivenUuid qtnUuid userDto questionnaireCreateDto

createQuestionnaireWithGivenUuid ::
     U.UUID -> UserDTO -> QuestionnaireCreateDTO -> AppContextM (Either AppError QuestionnaireDTO)
createQuestionnaireWithGivenUuid qtnUuid userDto reqDto =
  heFindPackageWithEventsById (reqDto ^. packageId) $ \package ->
    getEvents (reqDto ^. packageId) $ \events ->
      getKnowledgeModel events $ \knowledgeModel -> do
        now <- liftIO getCurrentTime
        let qtn = fromQuestionnaireCreateDTO reqDto qtnUuid knowledgeModel (userDto ^. uuid) now now
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

getQuestionnaireById :: String -> UserDTO -> AppContextM (Either AppError QuestionnaireDTO)
getQuestionnaireById qtnUuid userDto =
  heFindQuestionnaire $ \qtn -> heFindPackageById (qtn ^. packageId) $ \package -> return . Right $ toDTO qtn package
  where
    heFindQuestionnaire =
      if userDto ^. role == "ADMIN"
        then heFindQuestionnaireById qtnUuid
        else heFindQuestionnaireByIdAndOwnerUuid qtnUuid (U.toString $ userDto ^. uuid)

getQuestionnaireDetailById :: String -> UserDTO -> AppContextM (Either AppError QuestionnaireDetailDTO)
getQuestionnaireDetailById qtnUuid userDto =
  heFindQuestionnaire $ \qtn ->
    heFindPackageWithEventsById (qtn ^. packageId) $ \package ->
      return . Right $ toDetailWithPackageWithEventsDTO qtn package
  where
    heFindQuestionnaire =
      if userDto ^. role == "ADMIN"
        then heFindQuestionnaireById qtnUuid
        else heFindQuestionnaireByIdAndOwnerUuid qtnUuid (U.toString $ userDto ^. uuid)

modifyQuestionnaire ::
     String -> UserDTO -> QuestionnaireChangeDTO -> AppContextM (Either AppError QuestionnaireDetailDTO)
modifyQuestionnaire qtnUuid userDto reqDto =
  heGetQuestionnaireDetailById qtnUuid userDto $ \qtnDto -> do
    now <- liftIO getCurrentTime
    let updateQtn = fromChangeDTO qtnDto reqDto now
    updateQuestionnaireById updateQtn
    return . Right $ toDetailWithPackageDTODTO updateQtn (qtnDto ^. package)

deleteQuestionnaire :: String -> UserDTO -> AppContextM (Maybe AppError)
deleteQuestionnaire qtnUuid userDto = do
  eitherQuestionnaire <- heFindQuestionnaire
  case eitherQuestionnaire of
    Right questionnaire -> do
      deleteQuestionnaireById qtnUuid
      return Nothing
    Left error -> return . Just $ error
  where
    heFindQuestionnaire =
      if userDto ^. role == "ADMIN"
        then findQuestionnaireById qtnUuid
        else findQuestionnaireByIdAndOwnerUuid qtnUuid (U.toString $ userDto ^. uuid)

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetQuestionnaires callback = do
  eitherQuestionnaires <- getQuestionnaires
  case eitherQuestionnaires of
    Right questionnaires -> callback questionnaires
    Left error -> return . Left $ error

heGetQuestionnaireDetailById qtnUuid ownerUuid callback = do
  eitherQuestionnaire <- getQuestionnaireDetailById qtnUuid ownerUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Left $ error
