module Wizard.Database.Migration.Development.Dev.Data.Devs where

import Wizard.Api.Resource.Dev.DevExecutionDTO
import Wizard.Api.Resource.Dev.DevExecutionResultDTO
import Wizard.Model.Dev.Dev

section :: DevSection
section = DevSection {name = "Cache", description = Nothing, operations = [operation]}

operation :: DevOperation
operation =
  DevOperation
    { name = "purgeCache"
    , description = Nothing
    , parameters = [operationParam1]
    }

operationParam1 :: DevOperationParameter
operationParam1 =
  DevOperationParameter
    { name = "firstParam"
    , aType = StringDevOperationParameterType
    }

execution1 :: DevExecutionDTO
execution1 =
  DevExecutionDTO
    { sectionName = section.name
    , operationName = operation.name
    , parameters = []
    }

execution1Result :: AdminExecutionResultDTO
execution1Result = AdminExecutionResultDTO {output = "My output"}
