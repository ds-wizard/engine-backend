module Wizard.Service.Migration.Questionnaire.MigratorService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (liftIO)
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Migration.Questionnaire.Migrator.Sanitizator
import Wizard.Service.Migration.Questionnaire.MigratorMapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QM
import Wizard.Service.Questionnaire.QuestionnaireService

createQuestionnaireMigration :: String -> MigratorStateCreateDTO -> AppContextM MigratorStateDTO
createQuestionnaireMigration oldQtnUuid reqDto = do
  oldQtn <- getQuestionnaireDetailById oldQtnUuid
  checkMigrationPermissionToQtn (oldQtn ^. visibility) (oldQtn ^. ownerUuid)
  newQtn <- upgradeQuestionnaire reqDto (QM.fromDetailDTO oldQtn)
  insertQuestionnaire newQtn
  let state = fromCreateDTO (oldQtn ^. uuid) (newQtn ^. uuid)
  insertMigratorState state
  getQuestionnaireMigration (U.toString $ newQtn ^. uuid)

getQuestionnaireMigration :: String -> AppContextM MigratorStateDTO
getQuestionnaireMigration qtnUuid = do
  state <- findMigratorStateByNewQuestionnaireId qtnUuid
  oldQtn <- getQuestionnaireDetailById (U.toString $ state ^. oldQuestionnaireUuid)
  newQtn <- getQuestionnaireDetailById (U.toString $ state ^. newQuestionnaireUuid)
  checkMigrationPermissionToQtn (oldQtn ^. visibility) (oldQtn ^. ownerUuid)
  checkMigrationPermissionToQtn (newQtn ^. visibility) (newQtn ^. ownerUuid)
  return $ toDTO oldQtn newQtn (state ^. resolvedQuestionUuids)

modifyQuestionnaireMigration :: String -> MigratorStateChangeDTO -> AppContextM MigratorStateDTO
modifyQuestionnaireMigration qtnUuid reqDto = do
  state <- getQuestionnaireMigration qtnUuid
  let updatedState = fromChangeDTO reqDto state
  updateMigratorStateByNewQuestionnaireId updatedState
  return $ toDTO (state ^. oldQuestionnaire) (state ^. newQuestionnaire) (updatedState ^. resolvedQuestionUuids)

finishQuestionnaireMigration :: String -> AppContextM ()
finishQuestionnaireMigration qtnUuid = do
  _ <- getQuestionnaireMigration qtnUuid
  deleteMigratorStateByNewQuestionnaireId qtnUuid
  return ()

cancelQuestionnaireMigration :: String -> AppContextM ()
cancelQuestionnaireMigration qtnUuid = do
  state <- getQuestionnaireMigration qtnUuid
  deleteQuestionnaire (U.toString $ state ^. newQuestionnaire . uuid)
  deleteMigratorStateByNewQuestionnaireId qtnUuid
  return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
upgradeQuestionnaire :: MigratorStateCreateDTO -> Questionnaire -> AppContextM Questionnaire
upgradeQuestionnaire reqDto oldQtn = do
  let newPkgId = reqDto ^. targetPackageId
  let newTagUuids = reqDto ^. targetTagUuids
  oldKm <- compileKnowledgeModel [] (Just $ oldQtn ^. packageId) newTagUuids
  newKm <- compileKnowledgeModel [] (Just newPkgId) newTagUuids
  newUuid <- liftIO generateUuid
  let newReplies = sanitizeReplies oldKm newKm (oldQtn ^. replies)
  return $ (((oldQtn & uuid .~ newUuid) & packageId .~ newPkgId) & replies .~ newReplies) & selectedTagUuids .~
    newTagUuids
