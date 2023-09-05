module Shared.Common.Service.Dev.DevOperationService where

import Control.Monad.Except (throwError)

import Shared.Common.Api.Resource.Dev.DevExecutionDTO
import Shared.Common.Api.Resource.Dev.DevExecutionResultDTO
import Shared.Common.Api.Resource.Dev.DevSectionDTO
import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Context.AppContext
import Shared.Common.Model.Dev.Dev
import Shared.Common.Model.Error.Error
import Shared.Common.Service.Acl.AclService
import Shared.Common.Service.Dev.DevOperationMapper

getDevOperations :: AppContextC s sc m => [DevSection m] -> m [DevSectionDTO]
getDevOperations sections = do
  checkPermission _DEV_PERM
  return . fmap toDevSectionDTO $ sections

executeOperation :: AppContextC s sc m => [DevSection m] -> DevExecutionDTO -> m AdminExecutionResultDTO
executeOperation sections reqDto = do
  checkPermission _DEV_PERM
  case filter (\s -> s.name == reqDto.sectionName) sections of
    [section] ->
      case filter (\o -> o.name == reqDto.operationName) section.operations of
        [operation] -> do
          result <- operation.function reqDto
          return . AdminExecutionResultDTO $ result
        _ -> throwError . GeneralServerError $ _ERROR_SERVICE_DEV__INVALID_DEV_OPERATION_DEFINITIONS_OR_REQUEST
    _ -> throwError . GeneralServerError $ _ERROR_SERVICE_DEV__INVALID_DEV_OPERATION_DEFINITIONS_OR_REQUEST
