module Wizard.Database.Migration.Development.Admin.Data.Admins where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Admin.AdminExecutionDTO
import Wizard.Api.Resource.Admin.AdminExecutionResultDTO
import Wizard.Model.Admin.Admin

section :: AdminSection
section =
  AdminSection {_adminSectionName = "Cache", _adminSectionDescription = Nothing, _adminSectionOperations = [operation]}

operation :: AdminOperation
operation =
  AdminOperation
    { _adminOperationName = "purgeCache"
    , _adminOperationDescription = Nothing
    , _adminOperationParameters = [operationParam1]
    }

operationParam1 :: AdminOperationParameter
operationParam1 =
  AdminOperationParameter
    {_adminOperationParameterName = "firstParam", _adminOperationParameterAType = StringAdminOperationParameterType}

execution1 :: AdminExecutionDTO
execution1 =
  AdminExecutionDTO
    { _adminExecutionDTOSectionName = section ^. name
    , _adminExecutionDTOOperationName = operation ^. name
    , _adminExecutionDTOParameters = []
    }

execution1Result :: AdminExecutionResultDTO
execution1Result = AdminExecutionResultDTO {_adminExecutionResultDTOOutput = "My output"}
