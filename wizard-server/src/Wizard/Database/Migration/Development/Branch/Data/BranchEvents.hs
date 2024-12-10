module Wizard.Database.Migration.Development.Branch.Data.BranchEvents where

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Branch.Event.BranchEventDTO
import Wizard.Api.Resource.Branch.Event.SetRepliesDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Event

branchEvent1' :: BranchEventDTO
branchEvent1' = AddBranchEventDTO' branchEvent1

branchEvent1 :: AddBranchEventDTO
branchEvent1 =
  AddBranchEventDTO
    { uuid = u' "6858b0b6-bb6f-4e21-a0c2-6afc84950f7a"
    , event = AddKnowledgeModelEvent' a_km1
    }

setRepliesDTO :: SetRepliesDTO
setRepliesDTO =
  SetRepliesDTO
    { uuid = u' "91863e00-ae98-4bcf-aae6-03a5a801b4fa"
    , replies = fReplies
    }
