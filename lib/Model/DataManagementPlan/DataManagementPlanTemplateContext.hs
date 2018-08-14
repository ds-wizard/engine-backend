module Model.DataManagementPlan.DataManagementPlanTemplateContext where

import Data.Default
import Data.Text
import GHC.Generics


data DataManagementPlanTemplateContext = DataManagementPlanTemplateContext
  { _dataManagementPlanTemplateContextBaseURL :: Text
  , _dataManagementPlanTemplateContextResourcePageURL :: Text
  } deriving (Show, Generic)

instance Default DataManagementPlanTemplateContext where
  def = DataManagementPlanTemplateContext
    { _dataManagementPlanTemplateContextBaseURL = "https://dsw.fairdata.solutions"
    , _dataManagementPlanTemplateContextResourcePageURL = "https://app.dsw.fairdata.solutions/book-references/:shortuid"
    }
