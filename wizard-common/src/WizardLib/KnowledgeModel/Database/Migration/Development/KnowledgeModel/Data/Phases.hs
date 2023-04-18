module WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases where

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Util.Uuid
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

phase1 :: Phase
phase1 =
  Phase
    { uuid = u' "b101f2d0-2476-452d-aa8d-95a41a02b52c"
    , title = "Before Submitting the Proposal"
    , description = Nothing
    , annotations = []
    }

phase1Edited :: Phase
phase1Edited =
  phase1
    { title = "EDITED: Before Submitting the Proposal"
    , description = Just "EDITED: some description"
    , annotations = [MapEntry "newAnnotation" "someValue"]
    }

phase2 :: Phase
phase2 =
  Phase
    { uuid = u' "1796fa3c-9f53-475f-89ff-c66a0453c42e"
    , title = "Before Submitting the DMP"
    , description = Nothing
    , annotations = []
    }

phase3 :: Phase
phase3 =
  Phase
    { uuid = u' "adc9133d-afcd-4616-9aea-db5f475898a2"
    , title = "Before Finishing the Project"
    , description = Nothing
    , annotations = []
    }

phase4 :: Phase
phase4 =
  Phase
    { uuid = u' "1ace0fc6-a949-495f-a32e-e948f3f6bed1"
    , title = "After Finishing the Project"
    , description = Nothing
    , annotations = []
    }
