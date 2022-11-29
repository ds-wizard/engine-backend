module Wizard.Database.Migration.Development.Branch.Data.BranchEvents where

import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Event
import Shared.Util.Uuid
import Wizard.Api.Resource.Branch.Event.BranchEventDTO

branchEvent1' :: BranchEventDTO
branchEvent1' = AddBranchEventDTO' branchEvent1

branchEvent1 :: AddBranchEventDTO
branchEvent1 =
  AddBranchEventDTO
    { uuid = u' "6858b0b6-bb6f-4e21-a0c2-6afc84950f7a"
    , event = AddKnowledgeModelEvent' a_km1
    }
