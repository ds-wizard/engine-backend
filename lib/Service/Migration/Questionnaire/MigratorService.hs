module Service.Migration.Questionnaire.MigratorService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (liftIO)
import qualified Data.UUID as U

import Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Database.DAO.Migration.Questionnaire.MigratorDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Questionnaire.Questionnaire
import Service.KnowledgeModel.KnowledgeModelService
import Service.Migration.Questionnaire.MigratorMapper
import Service.Migration.Questionnaire.MigratorValidation
import Service.Migration.Questionnaire.Sanitizator
import qualified Service.Questionnaire.QuestionnaireMapper as QM
import Service.Questionnaire.QuestionnaireService
import Util.Helper (createHeeHelper, createHemHelper)
import Util.Uuid

createQuestionnaireMigration :: String -> MigratorStateCreateDTO -> AppContextM (Either AppError MigratorStateDTO)
createQuestionnaireMigration oldQtnUuid reqDto =
  heValidateQuestionnaireMigrationNotExist oldQtnUuid $ heGetQuestionnaireDetailById oldQtnUuid $ \oldQtn ->
    heCheckMigrationPermissionToQtn oldQtn $ heUpgradeQuestionnaire reqDto (QM.fromDetailDTO oldQtn) $ \newQtn -> do
      insertQuestionnaire newQtn
      let state = fromCreateDTO (oldQtn ^. uuid) (newQtn ^. uuid)
      createMigratorState state
      getQuestionnaireMigration (U.toString $ newQtn ^. uuid)

getQuestionnaireMigration :: String -> AppContextM (Either AppError MigratorStateDTO)
getQuestionnaireMigration qtnUuid =
  heFindMigratorStateByNewQuestionnaireId qtnUuid $ \state ->
    heGetQuestionnaireDetailById (U.toString $ state ^. oldQuestionnaireUuid) $ \oldQtn ->
      heGetQuestionnaireDetailById (U.toString $ state ^. newQuestionnaireUuid) $ \newQtn ->
        heCheckMigrationPermissionToQtn oldQtn $ heCheckMigrationPermissionToQtn newQtn $ do
          return . Right $ toDTO oldQtn newQtn (state ^. resolvedQuestionUuids)

modifyQuestionnaireMigration :: String -> MigratorStateChangeDTO -> AppContextM (Either AppError MigratorStateDTO)
modifyQuestionnaireMigration qtnUuid reqDto =
  heGetQuestionnaireMigration qtnUuid $ \state -> do
    let updatedState = fromChangeDTO reqDto state
    updateMigratorStateByNewQuestionnaireId updatedState
    return . Right $
      toDTO (state ^. oldQuestionnaire) (state ^. newQuestionnaire) (updatedState ^. resolvedQuestionUuids)

finishQuestionnaireMigration :: String -> AppContextM (Maybe AppError)
finishQuestionnaireMigration qtnUuid =
  hmGetQuestionnaireMigration qtnUuid $ \_ -> do
    deleteMigratorStateByNewQuestionnaireId qtnUuid
    return Nothing

cancelQuestionnaireMigration :: String -> AppContextM (Maybe AppError)
cancelQuestionnaireMigration qtnUuid =
  hmGetQuestionnaireMigration qtnUuid $ \state ->
    hmDeleteQuestionnaire (U.toString $ state ^. newQuestionnaire . uuid) $ do
      deleteMigratorStateByNewQuestionnaireId qtnUuid
      return Nothing

-- --------------------------------
-- PRIVATE
-- --------------------------------
upgradeQuestionnaire :: MigratorStateCreateDTO -> Questionnaire -> AppContextM (Either AppError Questionnaire)
upgradeQuestionnaire reqDto oldQtn = do
  let newPkgId = reqDto ^. targetPackageId
  let newTagUuids = reqDto ^. targetTagUuids
  heCompileKnowledgeModel [] (Just newPkgId) newTagUuids $ \km -> do
    newUuid <- liftIO generateUuid
    let newReplies = sanitizeReplies km (oldQtn ^. replies)
    return . Right $ (((oldQtn & uuid .~ newUuid) & packageId .~ newPkgId) & replies .~ newReplies) & selectedTagUuids .~
      newTagUuids

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetQuestionnaireMigration qtnUuid = createHeeHelper (getQuestionnaireMigration qtnUuid)

hmGetQuestionnaireMigration qtnUuid = createHemHelper (getQuestionnaireMigration qtnUuid)

-- -----------------------------------------------------
heUpgradeQuestionnaire targetPkgId oldQtn = createHeeHelper (upgradeQuestionnaire targetPkgId oldQtn)
