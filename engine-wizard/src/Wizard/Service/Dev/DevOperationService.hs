module Wizard.Service.Dev.DevOperationService (
  getDevOperations,
  executeOperation,
) where

import Wizard.Api.Resource.Dev.DevExecutionDTO
import Wizard.Api.Resource.Dev.DevExecutionResultDTO
import Wizard.Model.Context.AppContext hiding (cache)
import Wizard.Model.Dev.Dev
import Wizard.Service.Acl.AclService
import Wizard.Service.Dev.DevOperationDefinitions

getDevOperations :: AppContextM [DevSection]
getDevOperations = do
  checkPermission _DEV_PERM
  return [actionKey, appPlan, branch, cache, config, document, feedback, owl, persistentCommand, registry, questionnaire, temporaryFile, user]

executeOperation :: DevExecutionDTO -> AppContextM AdminExecutionResultDTO
executeOperation reqDto = do
  checkPermission _DEV_PERM
  result <- execute reqDto
  return . AdminExecutionResultDTO $ result

execute :: DevExecutionDTO -> AppContextM String
execute reqDto
  | action reqDto actionKey actionKey_cleanActionKeys = actionKey_cleanActionKeysFn reqDto
  | action reqDto appPlan appPlan_recomputePlansForApps = appPlan_recomputePlansForAppsFn reqDto
  | action reqDto branch branch_squashAllEvents = branch_squashAllEventsFn reqDto
  | action reqDto branch branch_squashEventsForBranch = branch_squashEventsForBranchFn reqDto
  | action reqDto cache cache_purgeCache = cache_purgeCacheFn reqDto
  | action reqDto config config_recompileCssInAllApplications = config_recompileCssInAllApplicationsFn reqDto
  | action reqDto config config_switchClientCustomizationOn = config_switchClientCustomizationOnFn reqDto
  | action reqDto config config_switchClientCustomizationOff = config_switchClientCustomizationOffFn reqDto
  | action reqDto document document_cleanDocuments = document_cleanDocumentsFn reqDto
  | action reqDto feedback feedback_synchronizeFeedbacks = feedback_synchronizeFeedbacksFn reqDto
  | action reqDto owl owl_switchOwlOn = owl_switchOwlOnFn reqDto
  | action reqDto owl owl_switchOwlOff = owl_switchOwlOffFn reqDto
  | action reqDto owl owl_setOwlProperties = owl_setOwlPropertiesFn reqDto
  | action reqDto persistentCommand persistentCommand_runAll = persistentCommand_runAllFn reqDto
  | action reqDto persistentCommand persistentCommand_run = persistentCommand_runFn reqDto
  | action reqDto registry registry_syncWithRegistry = registry_syncWithRegistryFn reqDto
  | action reqDto registry registry_pushPackageBundle = registry_pushPackageBundleFn reqDto
  | action reqDto registry registry_pushDocumentTemplateBundle = registry_pushDocumentTemplateBundleFn reqDto
  | action reqDto registry registry_pushLocaleBundle = registry_pushLocaleBundleFn reqDto
  | action reqDto questionnaire questionnaire_cleanQuestionnaires = questionnaire_cleanQuestionnairesFn reqDto
  | action reqDto questionnaire questionnaire_recomputeQuestionnaireIndications = questionnaire_recomputeQuestionnaireIndicationsFn reqDto
  | action reqDto questionnaire questionnaire_squashAllEvents = questionnaire_squashAllEventsFn reqDto
  | action reqDto questionnaire questionnaire_squashEventsForQuestionnaire = questionnaire_squashEventsForQuestionnaireFn reqDto
  | action reqDto temporaryFile temporaryFile_cleanTemporaryFiles = temporaryFile_cleanTemporaryFilesFn reqDto
  | action reqDto user user_cleanTokens = user_cleanTokensFn reqDto

action :: DevExecutionDTO -> DevSection -> DevOperation -> Bool
action reqDto section operation = reqDto.sectionName == section.name && reqDto.operationName == operation.name
