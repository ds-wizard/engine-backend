module Service.DataManagementPlan.DataManagementPlanMapper where

import Control.Lens ((^.))

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import LensesConfig
import Model.DataManagementPlan.DataManagementPlan
import Service.FilledKnowledgeModel.FilledKnowledgeModelMapper

toDTO :: DataManagementPlan -> DataManagementPlanDTO
toDTO dmp =
  DataManagementPlanDTO
  { _dataManagementPlanDTOUuid = dmp ^. uuid
  , _dataManagementPlanDTOQuestionnaireUuid = dmp ^. questionnaireUuid
  , _dataManagementPlanDTOFilledKnowledgeModel = toFilledKMDTO $ dmp ^. filledKnowledgeModel
  , _dataManagementPlanDTOCreatedAt = dmp ^. createdAt
  , _dataManagementPlanDTOUpdatedAt = dmp ^. updatedAt
  }
