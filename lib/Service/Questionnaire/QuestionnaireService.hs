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
import Model.Context.AppContextHelpers
import Model.Error.Error
import Model.Package.Package
import Model.Questionnaire.Questionnaire
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Package.PackageService
import Service.Questionnaire.QuestionnaireMapper
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
              let qtnDTO = toDTO qtn pkg
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
      questionnaire ^. private == False || questionnaire ^. ownerUuid == (Just $ currentUser ^. uuid)

createQuestionnaire :: QuestionnaireCreateDTO -> AppContextM (Either AppError QuestionnaireDTO)
createQuestionnaire questionnaireCreateDto = do
  qtnUuid <- liftIO generateUuid
  createQuestionnaireWithGivenUuid qtnUuid questionnaireCreateDto

createQuestionnaireWithGivenUuid :: U.UUID -> QuestionnaireCreateDTO -> AppContextM (Either AppError QuestionnaireDTO)
createQuestionnaireWithGivenUuid qtnUuid reqDto =
  heGetCurrentUser $ \currentUser ->
    heFindPackageWithEventsById (reqDto ^. packageId) $ \package ->
      heGetAllPreviousEventsSincePackageId (reqDto ^. packageId) $ \events ->
        heCreateKnowledgeModel events $ \knowledgeModel -> do
          now <- liftIO getCurrentTime
          let qtn = fromQuestionnaireCreateDTO reqDto qtnUuid knowledgeModel (currentUser ^. uuid) now now
          insertQuestionnaire qtn
          return . Right $ toSimpleDTO qtn package

getQuestionnaireById :: String -> AppContextM (Either AppError QuestionnaireDTO)
getQuestionnaireById qtnUuid =
  heGetCurrentUser $ \currentUser ->
    heFindQuestionnaire currentUser $ \qtn ->
      heFindPackageById (qtn ^. packageId) $ \package -> return . Right $ toDTO qtn package
  where
    heFindQuestionnaire currentUser =
      if currentUser ^. role == "ADMIN"
        then heFindQuestionnaireById qtnUuid
        else heFindQuestionnaireByIdAndOwnerUuid qtnUuid (U.toString $ currentUser ^. uuid)

getQuestionnaireDetailById :: String -> AppContextM (Either AppError QuestionnaireDetailDTO)
getQuestionnaireDetailById qtnUuid =
  heGetCurrentUser $ \currentUser ->
    heFindQuestionnaire currentUser $ \qtn ->
      heFindPackageWithEventsById (qtn ^. packageId) $ \package ->
        return . Right $ toDetailWithPackageWithEventsDTO qtn package
  where
    heFindQuestionnaire currentUser =
      if currentUser ^. role == "ADMIN"
        then heFindQuestionnaireById qtnUuid
        else heFindQuestionnaireByIdAndOwnerUuid qtnUuid (U.toString $ currentUser ^. uuid)

modifyQuestionnaire :: String -> QuestionnaireChangeDTO -> AppContextM (Either AppError QuestionnaireDetailDTO)
modifyQuestionnaire qtnUuid reqDto =
  heGetCurrentUser $ \currentUser ->
    heGetQuestionnaireDetailById qtnUuid $ \qtnDto -> do
      now <- liftIO getCurrentTime
      let updatedQtn = fromChangeDTO qtnDto reqDto now
      updateQuestionnaireById updatedQtn
      return . Right $ toDetailWithPackageDTODTO updatedQtn (qtnDto ^. package)

deleteQuestionnaire :: String -> AppContextM (Maybe AppError)
deleteQuestionnaire qtnUuid =
  hmGetCurrentUser $ \currentUser ->
    hmFindQuestionnaire currentUser $ \questionnaire -> do
      deleteQuestionnaireById qtnUuid
      return Nothing
  where
    hmFindQuestionnaire currentUser =
      if currentUser ^. role == "ADMIN"
        then hmFindQuestionnaireById qtnUuid
        else hmFindQuestionnaireByIdAndOwnerUuid qtnUuid (U.toString $ currentUser ^. uuid)

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetQuestionnaires callback = do
  eitherQuestionnaires <- getQuestionnaires
  case eitherQuestionnaires of
    Right questionnaires -> callback questionnaires
    Left error -> return . Left $ error

heGetQuestionnaireDetailById qtnUuid callback = do
  eitherQuestionnaire <- getQuestionnaireDetailById qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Left $ error
