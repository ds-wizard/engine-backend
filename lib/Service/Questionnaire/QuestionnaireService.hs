module Service.Questionnaire.QuestionnaireService where

import Control.Lens ((^.))
import Control.Monad.Reader
import qualified Data.UUID as U

import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Common.Error
import Common.Uuid
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Model.Context.AppContext
import Model.Package.Package
import Model.Questionnaire.Questionnaire
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Package.PackageService
import Service.Questionnaire.QuestionnaireMapper

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
  uuid <- liftIO generateUuid
  createQuestionnaireWithGivenUuid uuid questionnaireCreateDto

createQuestionnaireWithGivenUuid :: U.UUID -> QuestionnaireCreateDTO -> AppContextM (Either AppError QuestionnaireDTO)
createQuestionnaireWithGivenUuid qtnUuid reqDto =
  getPackage (reqDto ^. packageId) $ \package ->
    getEvents (reqDto ^. packageId) $ \events ->
      getKnowledgeModel events $ \knowledgeModel -> do
        let qtn = fromQuestionnaireCreateDTO reqDto qtnUuid knowledgeModel
        insertQuestionnaire qtn
        return . Right $ toSimpleDTO qtn package
  where
    getPackage pkgId callback = do
      eitherPackage <- findPackageWithEventsById pkgId
      case eitherPackage of
        Right package -> callback package
        Left error -> return . Left $ error
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

getQuestionnaireById :: String -> AppContextM (Either AppError QuestionnaireDetailDTO)
getQuestionnaireById qtnUuid =
  getQuestionnaire qtnUuid $ \qtn ->
    getPackage (qtn ^. packageId) $ \package -> return . Right $ toDetailDTO qtn package
  where
    getPackage pkgId callback = do
      eitherPackage <- findPackageWithEventsById pkgId
      case eitherPackage of
        Right package -> callback package
        Left error -> return . Left $ error
    getQuestionnaire qtnUuid callback = do
      eitherQuestionnaire <- findQuestionnaireById qtnUuid
      case eitherQuestionnaire of
        Right questionnaire -> callback questionnaire
        Left error -> return . Left $ error

modifyQuestionnaireReplies :: String -> QuestionnaireReplies -> AppContextM (Either AppError QuestionnaireReplies)
modifyQuestionnaireReplies qtnUuid qtnReplies =
  getQuestionnaire qtnUuid $ \qtn -> do
    updateQuestionnaireRepliesById qtnUuid qtnReplies
    return . Right $ qtnReplies
  where
    getQuestionnaire qtnUuid callback = do
      eitherQuestionnaire <- findQuestionnaireById qtnUuid
      case eitherQuestionnaire of
        Right questionnaire -> callback questionnaire
        Left error -> return . Left $ error

deleteQuestionnaire :: String -> AppContextM (Maybe AppError)
deleteQuestionnaire qtnUuid = do
  eitherQuestionnaire <- findQuestionnaireById qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> do
      deleteQuestionnaireById qtnUuid
      return Nothing
    Left error -> return . Just $ error
