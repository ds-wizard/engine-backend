module Wizard.Service.Admin.AdminService
  ( getAdminOperations
  , executeOperation
  ) where

import Control.Lens ((^.))

import LensesConfig hiding (action, cache, feedback)
import Wizard.Api.Resource.Admin.AdminExecutionDTO
import Wizard.Api.Resource.Admin.AdminExecutionResultDTO
import Wizard.Model.Admin.Admin
import Wizard.Model.Context.AppContext
import Wizard.Service.Acl.AclService
import Wizard.Service.Admin.AdminDefinition

getAdminOperations :: AppContextM [AdminSection]
getAdminOperations = return [cache, feedback]

executeOperation :: AdminExecutionDTO -> AppContextM AdminExecutionResultDTO
executeOperation reqDto = do
  checkPermission _ADMIN_PERM
  result <- execute reqDto
  return . AdminExecutionResultDTO $ result

execute :: AdminExecutionDTO -> AppContextM String
execute reqDto
  | action reqDto cache cache_purgeCache = cache_purgeCacheFn reqDto
  | action reqDto cache cache_KnowledgeModelCache_deleteFromCache' = cache_KnowledgeModelCache_deleteFromCacheFn' reqDto
  | action reqDto feedback feedback_synchronizeFeedbacks = feedback_synchronizeFeedbacksFn reqDto

action :: AdminExecutionDTO -> AdminSection -> AdminOperation -> Bool
action reqDto section operation =
  (reqDto ^. sectionName) == (section ^. name) && (reqDto ^. operationName) == (operation ^. name)
