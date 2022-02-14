module Wizard.Service.Admin.AdminService
  ( getAdminOperations
  , executeOperation
  ) where

import Control.Lens ((^.))

import LensesConfig hiding (action, branch, cache, config, feedback, persistentCommand, questionnaire)
import Wizard.Api.Resource.Admin.AdminExecutionDTO
import Wizard.Api.Resource.Admin.AdminExecutionResultDTO
import Wizard.Model.Admin.Admin
import Wizard.Model.Context.AppContext
import Wizard.Service.Acl.AclService
import Wizard.Service.Admin.AdminDefinition

getAdminOperations :: AppContextM [AdminSection]
getAdminOperations = return [app, branch, cache, config, feedback, persistentCommand, questionnaire]

executeOperation :: AdminExecutionDTO -> AppContextM AdminExecutionResultDTO
executeOperation reqDto = do
  checkPermission _ADMIN_PERM
  result <- execute reqDto
  return . AdminExecutionResultDTO $ result

execute :: AdminExecutionDTO -> AppContextM String
execute reqDto
  | action reqDto app app_createApp = app_createAppFn reqDto
  | action reqDto app app_createCustomApp = app_createCustomAppFn reqDto
  | action reqDto branch branch_squashAllEvents = branch_squashAllEventsFn reqDto
  | action reqDto branch branch_squashEventsForBranch = branch_squashEventsForBranchFn reqDto
  | action reqDto cache cache_purgeCache = cache_purgeCacheFn reqDto
  | action reqDto cache cache_KnowledgeModelCache_deleteFromCache' = cache_KnowledgeModelCache_deleteFromCacheFn' reqDto
  | action reqDto config config_recompileCssInAllApplications = config_recompileCssInAllApplicationsFn reqDto
  | action reqDto config config_switchClientCustomizationOn = config_switchClientCustomizationOnFn reqDto
  | action reqDto config config_switchClientCustomizationOff = config_switchClientCustomizationOffFn reqDto
  | action reqDto feedback feedback_synchronizeFeedbacks = feedback_synchronizeFeedbacksFn reqDto
  | action reqDto persistentCommand persistentCommand_runAll = persistentCommand_runAllFn reqDto
  | action reqDto persistentCommand persistentCommand_run = persistentCommand_runFn reqDto
  | action reqDto questionnaire questionnaire_squashAllEvents = questionnaire_squashAllEventsFn reqDto
  | action reqDto questionnaire questionnaire_squashEventsForQuestionnaire =
    questionnaire_squashEventsForQuestionnaireFn reqDto

action :: AdminExecutionDTO -> AdminSection -> AdminOperation -> Bool
action reqDto section operation =
  (reqDto ^. sectionName) == (section ^. name) && (reqDto ^. operationName) == (operation ^. name)
