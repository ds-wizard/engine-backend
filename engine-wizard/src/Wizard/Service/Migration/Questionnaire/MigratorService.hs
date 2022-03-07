module Wizard.Service.Migration.Questionnaire.MigratorService where

import Control.Lens ((.~), (^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Service.Acl.AclService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Migration.Questionnaire.Migrator.Sanitizator
import Wizard.Service.Migration.Questionnaire.MigratorMapper
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireService

createQuestionnaireMigration :: String -> MigratorStateCreateDTO -> AppContextM MigratorStateDTO
createQuestionnaireMigration oldQtnUuid reqDto =
  runInTransaction $ do
    checkPermission _QTN_PERM
    oldQtn <- findQuestionnaireById oldQtnUuid
    checkMigrationPermissionToQtn (oldQtn ^. visibility) (oldQtn ^. permissions)
    newQtn <- upgradeQuestionnaire reqDto oldQtn
    insertQuestionnaire newQtn
    appUuid <- asks _appContextAppUuid
    let state = fromCreateDTO (oldQtn ^. uuid) (newQtn ^. uuid) appUuid
    insertMigratorState state
    getQuestionnaireMigration (U.toString $ newQtn ^. uuid)

getQuestionnaireMigration :: String -> AppContextM MigratorStateDTO
getQuestionnaireMigration qtnUuid =
  runInTransaction $ do
    checkPermission _QTN_PERM
    state <- findMigratorStateByNewQuestionnaireId qtnUuid
    oldQtnDto <- getQuestionnaireDetailById (U.toString $ state ^. oldQuestionnaireUuid)
    newQtnDto <- getQuestionnaireDetailById (U.toString $ state ^. newQuestionnaireUuid)
    oldQtn <- findQuestionnaireById (U.toString $ state ^. oldQuestionnaireUuid)
    newQtn <- findQuestionnaireById (U.toString $ state ^. newQuestionnaireUuid)
    checkMigrationPermissionToQtn (oldQtn ^. visibility) (oldQtn ^. permissions)
    checkMigrationPermissionToQtn (newQtn ^. visibility) (newQtn ^. permissions)
    return $ toDTO oldQtnDto newQtnDto (state ^. resolvedQuestionUuids) (state ^. appUuid)

modifyQuestionnaireMigration :: String -> MigratorStateChangeDTO -> AppContextM MigratorStateDTO
modifyQuestionnaireMigration qtnUuid reqDto =
  runInTransaction $ do
    checkPermission _QTN_PERM
    state <- getQuestionnaireMigration qtnUuid
    let updatedState = fromChangeDTO reqDto state
    updateMigratorStateByNewQuestionnaireId updatedState
    return $
      toDTO
        (state ^. oldQuestionnaire)
        (state ^. newQuestionnaire)
        (updatedState ^. resolvedQuestionUuids)
        (updatedState ^. appUuid)

finishQuestionnaireMigration :: String -> AppContextM ()
finishQuestionnaireMigration qtnUuid =
  runInTransaction $ do
    checkPermission _QTN_PERM
    _ <- getQuestionnaireMigration qtnUuid
    state <- findMigratorStateByNewQuestionnaireId qtnUuid
    deleteMigratorStateByNewQuestionnaireId qtnUuid
    oldQtn <- findQuestionnaireById (U.toString $ state ^. oldQuestionnaireUuid)
    newQtn <- findQuestionnaireById (U.toString $ state ^. newQuestionnaireUuid)
    let updatedQtn =
          (packageId .~ (newQtn ^. packageId)) . (events .~ (newQtn ^. events)) .
          (selectedQuestionTagUuids .~ (newQtn ^. selectedQuestionTagUuids)) .
          (templateId .~ (newQtn ^. templateId)) .
          (formatUuid .~ (newQtn ^. formatUuid)) $
          oldQtn
    updateQuestionnaireById updatedQtn
    deleteQuestionnaire (U.toString $ newQtn ^. uuid) False
    return ()

cancelQuestionnaireMigration :: String -> AppContextM ()
cancelQuestionnaireMigration qtnUuid =
  runInTransaction $ do
    checkPermission _QTN_PERM
    state <- getQuestionnaireMigration qtnUuid
    deleteQuestionnaire (U.toString $ state ^. newQuestionnaire . uuid) True
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
  newEvents <- sanitizeQuestionnaireEvents oldKm newKm (oldQtn ^. events)
  newPermissions <- traverse (upgradeQuestionnairePerm newUuid) (oldQtn ^. permissions)
  return $ (uuid .~ newUuid) . (packageId .~ newPkgId) . (events .~ newEvents) .
    (selectedQuestionTagUuids .~ newTagUuids) .
    (templateId .~ Nothing) .
    (formatUuid .~ Nothing) .
    (permissions .~ newPermissions) $
    oldQtn

upgradeQuestionnairePerm :: U.UUID -> QuestionnairePermRecord -> AppContextM QuestionnairePermRecord
upgradeQuestionnairePerm newQtnUuid perm = do
  newUuid <- liftIO generateUuid
  return . (uuid .~ newUuid) . (questionnaireUuid .~ newQtnUuid) $ perm
