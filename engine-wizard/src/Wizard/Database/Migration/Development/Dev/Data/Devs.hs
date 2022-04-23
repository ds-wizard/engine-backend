module Wizard.Database.Migration.Development.Dev.Data.Devs where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Dev.DevExecutionDTO
import Wizard.Api.Resource.Dev.DevExecutionResultDTO
import Wizard.Model.Dev.Dev

section :: DevSection
section = DevSection {_devSectionName = "Cache", _devSectionDescription = Nothing, _devSectionOperations = [operation]}

operation :: DevOperation
operation =
  DevOperation
    {_devOperationName = "purgeCache", _devOperationDescription = Nothing, _devOperationParameters = [operationParam1]}

operationParam1 :: DevOperationParameter
operationParam1 =
  DevOperationParameter
    {_devOperationParameterName = "firstParam", _devOperationParameterAType = StringDevOperationParameterType}

execution1 :: DevExecutionDTO
execution1 =
  DevExecutionDTO
    { _devExecutionDTOSectionName = section ^. name
    , _devExecutionDTOOperationName = operation ^. name
    , _devExecutionDTOParameters = []
    }

execution1Result :: AdminExecutionResultDTO
execution1Result = AdminExecutionResultDTO {_devExecutionResultDTOOutput = "My output"}
