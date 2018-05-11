module Service.Questionnaire.QuestionnaireService where

import Control.Lens ((^.))
import Control.Monad.Reader
import qualified Data.UUID as U

import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Common.Context
import Common.Error
import Common.Uuid
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Model.Config.DSWConfig
import Model.Package.Package
import Model.Questionnaire.Questionnaire
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Package.PackageService
import Service.Questionnaire.QuestionnaireMapper

getQuestionnaires :: Context -> IO (Either AppError [QuestionnaireDTO])
getQuestionnaires context = do
  eitherQuestionnaires <- findQuestionnaires context
  liftIO $ print eitherQuestionnaires
  case eitherQuestionnaires of
    Right questionnaires -> do
      let ioEitherQuestionnairesWithPackages = addPackage context <$> questionnaires
      Prelude.foldl foldFn (return . Right $ []) ioEitherQuestionnairesWithPackages
    Left error -> return . Left $ error
  where
    addPackage :: Context -> Questionnaire -> IO (Either AppError (Questionnaire, Package))
    addPackage context qtn = do
      eitherPackage <- findPackageById context (qtn ^. packageId)
      case eitherPackage of
        Right package -> return . Right $ (qtn, package)
        Left error -> return . Left $ error
    foldFn ::
         IO (Either AppError [QuestionnaireDTO])
      -> IO (Either AppError (Questionnaire, Package))
      -> IO (Either AppError [QuestionnaireDTO])
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

createQuestionnaire :: Context -> DSWConfig -> QuestionnaireCreateDTO -> IO (Either AppError QuestionnaireDTO)
createQuestionnaire context config questionnaireCreateDto = do
  uuid <- generateUuid
  createQuestionnaireWithGivenUuid context config uuid questionnaireCreateDto

createQuestionnaireWithGivenUuid ::
     Context -> DSWConfig -> U.UUID -> QuestionnaireCreateDTO -> IO (Either AppError QuestionnaireDTO)
createQuestionnaireWithGivenUuid context config qtnUuid reqDto =
  getPackage (reqDto ^. packageId) $ \package ->
    getEvents (reqDto ^. packageId) $ \events ->
      getKnowledgeModel events $ \knowledgeModel -> do
        let qtn = fromQuestionnaireCreateDTO reqDto qtnUuid knowledgeModel
        insertQuestionnaire context qtn
        return . Right $ toSimpleDTO qtn package
  where
    getPackage pkgId callback = do
      eitherPackage <- findPackageWithEventsById context pkgId
      case eitherPackage of
        Right package -> callback package
        Left error -> return . Left $ error
    getEvents pkgId callback = do
      eitherEvents <- getAllPreviousEventsSincePackageId context pkgId
      case eitherEvents of
        Right events -> callback events
        Left error -> return . Left $ error
    getKnowledgeModel events callback = do
      let eitherKm = createKnowledgeModel $ events
      case eitherKm of
        Right km -> callback km
        Left error -> return . Left $ error

getQuestionnaireById :: Context -> String -> IO (Either AppError QuestionnaireDetailDTO)
getQuestionnaireById context qtnUuid =
  getQuestionnaire qtnUuid $ \qtn ->
    getPackage (qtn ^. packageId) $ \package -> return . Right $ toDetailDTO qtn package
  where
    getPackage pkgId callback = do
      eitherPackage <- findPackageWithEventsById context pkgId
      case eitherPackage of
        Right package -> callback package
        Left error -> return . Left $ error
    getQuestionnaire qtnUuid callback = do
      eitherQuestionnaire <- findQuestionnaireById context qtnUuid
      case eitherQuestionnaire of
        Right questionnaire -> callback questionnaire
        Left error -> return . Left $ error

modifyQuestionnaireReplies :: Context -> String -> QuestionnaireReplies -> IO (Either AppError QuestionnaireReplies)
modifyQuestionnaireReplies context qtnUuid qtnReplies =
  getQuestionnaire qtnUuid $ \qtn -> do
    updateQuestionnaireRepliesById context qtnUuid qtnReplies
    return . Right $ qtnReplies
  where
    getQuestionnaire qtnUuid callback = do
      eitherQuestionnaire <- findQuestionnaireById context qtnUuid
      case eitherQuestionnaire of
        Right questionnaire -> callback questionnaire
        Left error -> return . Left $ error

deleteQuestionnaire :: Context -> String -> IO (Maybe AppError)
deleteQuestionnaire context qtnUuid = do
  eitherQuestionnaire <- findQuestionnaireById context qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> do
      deleteQuestionnaireById context qtnUuid
      return Nothing
    Left error -> return . Just $ error
